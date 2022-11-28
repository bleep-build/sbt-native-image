package bleep.plugin.nativeimage

import bleep._
import bleep.internal.FileUtils
import bleep.logging.Logger
import bloop.config.Config.Project

import java.io.File
import java.nio.file.{Files, Path}
import java.util.jar.{Attributes, JarOutputStream, Manifest}
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.sys.process.Process
import scala.util.Properties

class NativeImagePlugin(
    project: Project,
    logger: Logger,
    nativeImageJvm: model.Jvm,
    // Extra command-line arguments that should be forwarded to the native-image optimizer.
    nativeImageOptions: Seq[String] = Nil,
    executionContext: ExecutionContext = ExecutionContext.global,
    env: List[(String, String)] = Nil
) {

  val target = project.out / "target"
  val targetNativeImageInternal = target / "native-image-internal"
  val targetNativeImage = target / "native-image"

  // The binary that is produced by native-image
  def nativeImageOutput: Path = targetNativeImage / project.name

  final class MessageOnlyException(override val toString: String) extends RuntimeException(toString)

  // The command arguments to launch the GraalVM native-image binary
  lazy val nativeImageCommand: Path = {
    def cmd(cmd: String) = if (Properties.isWin) s"$cmd.cmd" else cmd
    val javaCmd = FetchJvm(new BleepCacheLogger(logger), nativeImageJvm, executionContext)

    val nativeImageCmd = javaCmd.resolveSibling(cmd("native-image"))
    if (!FileUtils.exists(nativeImageCmd)) {
      import scala.sys.process._
      List(javaCmd.resolveSibling(cmd("gu")).toString, "install", "native-image").!!
    }
    nativeImageCmd
  }

  //    // Run application, tracking all usages of dynamic features of an execution with `native-image-agent`.
  //    def nativeImageRunAgent: Unit = {
  //      val _ = nativeImageCommand
  //      val graalHome = nativeImageGraalHome.toFile
  //      val agentConfig = if (nativeImageAgentMerge) "config-merge-dir" else "config-output-dir"
  //      val agentOption = s"-agentlib:native-image-agent=$agentConfig=${nativeImageAgentOutputDir}"
  //      val tpr = thisProjectRef.value
  //      val settings = Seq(
  //        fork in (tpr, Compile, run) := true,
  //        javaHome in (tpr, Compile, run) := Some(graalHome),
  //        javaOptions in (tpr, Compile, run) += agentOption
  //      )
  //      val state0 = state.value
  //      val extracted = Project.extract(state0)
  //      val newState = extracted.append(settings, state0)
  //      val arguments = spaceDelimited("<arg>").parsed
  //      val input = if (arguments.isEmpty) "" else arguments.mkString(" ")
  //      Project
  //        .extract(newState)
  //        .runInputTask(run in (tpr, Compile), input, newState)
  //    }

  // Directory where `native-image-agent` should put generated configurations.
  def nativeImageAgentOutputDir: Path = target / "native-image-configs"

  // Whether `native-image-agent` should merge generated configurations.
  // See https://www.graalvm.org/reference-manual/native-image/BuildConfiguration/#assisted-configuration-of-native-image-builds for details
  def nativeImageAgentMerge: Boolean = false

  // Generate a native image for this project.
  def nativeImage(): Path = {
    val main = project.platform.flatMap(_.mainClass)
    val binaryName = nativeImageOutput

    val cp = fixedClasspath(project)

    // NOTE(olafur): we pass in a manifest jar instead of the full classpath
    // for two reasons:
    // * large classpaths quickly hit on the "argument list too large"
    //   error, especially on Windows.
    // * we print the full command to the console and the manifest jar makes
    //   it more readable and easier to copy-paste.
    val manifest = targetNativeImageInternal / "manifest.jar"
    Files.createDirectories(manifest.getParent)
    createManifestJar(manifest, cp)
    val nativeClasspath = manifest.toAbsolutePath.toString

    // Assemble native-image argument list.
    val command = mutable.ListBuffer.empty[String]
    command += nativeImageCommand.toString
    command += "-cp"
    command += nativeClasspath
    command ++= nativeImageOptions
    command +=
      main.getOrElse(
        throw new MessageOnlyException(
          "no mainClass is specified. " +
            "To fix this problem, update build.sbt to include the settings " +
            "`mainClass.in(Compile) := Some(\"com.MainClass\")`"
        )
      )
    command += binaryName.toAbsolutePath.toString

    // Start native-image linker.
    val cwd = targetNativeImage
    Files.createDirectories(cwd)
    cli(action = "ni", cwd = cwd, cmd = command.toList, logger = logger, out = cli.Out.ViaLogger(logger), env = env)
    logger.withContext(binaryName).info("Native image ready")
    binaryName
  }

  // Run the generated native-image binary without linking
  def nativeImageRun(arguments: List[String]): Unit = {
    val binary = nativeImageOutput
    if (!Files.isRegularFile(binary)) {
      throw new MessageOnlyException(
        s"no such file: $binary.\nTo fix this problem, run 'nativeImage' first."
      )
    }
    val exit = Process(binary.toAbsolutePath.toString :: arguments).!
    if (exit != 0) {
      throw new MessageOnlyException(s"non-zero exit: $exit")
    }
  }

  private def createManifestJar(manifestJar: Path, cp: Seq[Path]): Unit = {
    // Add trailing slash to directories so that manifest dir entries work
    val classpathStr = cp.map(addTrailingSlashToDirectories(manifestJar)).mkString(" ")
    val manifest = new Manifest()
    manifest.getMainAttributes.put(Attributes.Name.MANIFEST_VERSION, "1.0")
    manifest.getMainAttributes.put(Attributes.Name.CLASS_PATH, classpathStr)
    val out = Files.newOutputStream(manifestJar)
    // This needs to be declared since jos itself should be set to close as well.
    var jos: JarOutputStream = null
    try jos = new JarOutputStream(out, manifest)
    finally
      if (jos == null) {
        out.close()
      } else {
        jos.close()
      }
  }

  private def addTrailingSlashToDirectories(manifestJar: Path)(path: Path): String = {
    val syntax: String =
      if (Properties.isWin) {
        // NOTE(danirey): absolute paths are not supported by all JDKs on Windows, therefore using relative paths
        // relative paths may not be URL-encoded, otherwise an absolute path is constructed
        val manifestPath = manifestJar.getParent
        val dependencyPath = path
        try manifestPath.relativize(dependencyPath).toString
        catch {
          // java.lang.IllegalArgumentException: 'other' has different root
          // this happens if the dependency jar resides on a different drive then the manifest, i.e. C:\Coursier\Cache and D:\myapp\target
          // copy dependency next to manifest as fallback
          case _: IllegalArgumentException =>
            import java.nio.file.{Files, StandardCopyOption}
            Files.copy(
              dependencyPath,
              manifestPath.resolve(path.getFileName),
              StandardCopyOption.REPLACE_EXISTING
            )
            path.getFileName.toString
        }
      } else {
        // NOTE(olafur): manifest jars must use URL-encoded paths.
        // https://docs.oracle.com/javase/7/docs/technotes/guides/jar/jar.html
        path.toUri.getPath
      }

    val separatorAdded =
      if (syntax.endsWith(".jar") || syntax.endsWith(File.separator)) {
        syntax
      } else {
        syntax + File.separator
      }
    separatorAdded
  }
}
