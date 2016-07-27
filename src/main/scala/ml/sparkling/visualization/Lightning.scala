package ml.sparkling.visualization

import java.io.StringWriter

import org.apache.toree.interpreter.{ExecuteError, Results}
import org.apache.toree.magic.dependencies.{IncludeKernelInterpreter, IncludeOutputStream}
import org.apache.toree.magic.{CellMagic, CellMagicOutput}
import org.apache.toree.plugins.annotations.Event
import org.apache.toree.utils.ArgumentParsingSupport
import org.apache.toree.kernel.protocol.v5._
import org.viz.lightning.Visualization

import scala.util.Try

/**
  * Created by Roman Bartusiak riomus@gmail.com roman.bartusiak@pwr.edu.pl on 27.07.16.
  */
class Lightning extends CellMagic
  with IncludeKernelInterpreter
  with IncludeOutputStream
  with ArgumentParsingSupport {

  object Responses {
    val MagicAborted = s"${classOf[Lightning].getSimpleName} magic aborted!"


    def NoVariableFound(name: String) = {
      s"No variable found with the name ${name}!"
    }

    def ErrorMessage(error: String) = {
      s"An error occurred.\n${error}"
    }

    val Incomplete = "Lightning code was an incomplete code snippet"

    val Usage =
      """%%lightning [arguments]
        |VISUALIZATION_CODE
        |
        |VISUALIZATION_CODE can be any numbered lines of code, as long as the
        |last line is a reference to a variable which is a Visualization.
      """.stripMargin
  }

  private def helpToCellMagicOutput(optionalException: Option[Exception] = None): CellMagicOutput = {
    val stringWriter = new StringWriter()
    stringWriter.append(optionalException.map(e => {
      s"ERROR: ${e.getMessage}\n"
    }).getOrElse(""))
    stringWriter.write(Responses.Usage)
    parser.printHelpOn(stringWriter)
    CellMagicOutput(MIMEType.PlainText -> stringWriter.toString)
  }

  private def executeCode(rddCode: String): CellMagicOutput = {
    val (result, message) = kernelInterpreter.interpret(rddCode)
    result match {
      case Results.Success =>
        val rddVarName = kernelInterpreter.lastExecutionVariableName.get
        kernelInterpreter.read(rddVarName).map { case variableVal: Visualization => {
          CellMagicOutput(MIMEType.TextHtml -> variableVal.getHTML)
        }
        case _ => {
          val errorMessage = Responses.ErrorMessage("Last line do not contain Lightning visualization")
          CellMagicOutput(MIMEType.PlainText -> errorMessage)
        }
        }.getOrElse(CellMagicOutput(MIMEType.PlainText -> Responses.NoVariableFound(rddVarName)))
      case Results.Aborted =>
        CellMagicOutput(
          MIMEType.PlainText -> Responses.ErrorMessage(Responses.MagicAborted)
        )
      case Results.Error =>
        val error = message.right.get.asInstanceOf[ExecuteError]
        val errorMessage = Responses.ErrorMessage(error.value)
        CellMagicOutput(MIMEType.PlainText -> errorMessage)
      case Results.Incomplete =>
        CellMagicOutput(MIMEType.PlainText -> Responses.Incomplete)
    }
  }

  @Event(name = "visualization")
  override def execute(code: String): CellMagicOutput = {
    val lines = code.trim.split("\n")
    Try({
      val res: CellMagicOutput = if (lines.length == 1 && lines.head.length == 0) {
        helpToCellMagicOutput()
      } else {
        executeCode(code)
      }
      res
    }).recover({
      case e: Exception =>
        helpToCellMagicOutput(Some(e))
    }).get
  }

}
