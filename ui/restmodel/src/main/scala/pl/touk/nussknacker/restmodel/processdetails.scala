package pl.touk.nussknacker.restmodel

import java.time.LocalDateTime

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.generic.JsonCodec
import io.circe.java8.time.{JavaTimeDecoders, JavaTimeEncoders}
import pl.touk.nussknacker.engine.ProcessingTypeData.ProcessingType
import pl.touk.nussknacker.engine.api.process.{ProcessName, ProcessId => ApiProcessId}
import pl.touk.nussknacker.engine.canonicalgraph.CanonicalProcess
import pl.touk.nussknacker.restmodel.ProcessType.ProcessType
import pl.touk.nussknacker.restmodel.displayedgraph.{DisplayableProcess, ValidatedDisplayableProcess}
import pl.touk.nussknacker.restmodel.process.{ProcessId, ProcessIdWithName}
import pl.touk.nussknacker.restmodel.processdetails.ActionType.ActionType

object processdetails extends JavaTimeEncoders with JavaTimeDecoders {

  object BasicProcess {
    def apply[ProcessShape](baseProcessDetails: BaseProcessDetails[ProcessShape]) = new BasicProcess(
      id = ApiProcessId(baseProcessDetails.processId),
      name = ProcessName(baseProcessDetails.name),
      processVersionId = baseProcessDetails.processVersionId,
      isSubprocess = baseProcessDetails.isSubprocess,
      isArchived = baseProcessDetails.isArchived,
      processCategory = baseProcessDetails.processCategory,
      processType = baseProcessDetails.processType,
      processingType = baseProcessDetails.processingType,
      modificationDate = baseProcessDetails.modificationDate,
      createdAt = baseProcessDetails.createdAt,
      createdBy = baseProcessDetails.createdBy,
      lastAction = baseProcessDetails.lastAction,
      lastDeployedAction = baseProcessDetails.lastDeployedAction
    )
  }

  @JsonCodec case class BasicProcess(id: ApiProcessId,
                                     name: ProcessName,
                                     processVersionId: Long,
                                     isArchived: Boolean,
                                     isSubprocess: Boolean,
                                     processCategory: String,
                                     processType: ProcessType,
                                     processingType: ProcessingType,
                                     modificationDate: LocalDateTime,
                                     createdAt: LocalDateTime,
                                     createdBy: String,
                                     lastAction: Option[ProcessAction],
                                     lastDeployedAction: Option[ProcessAction]) {
    def isDeployed: Boolean = lastAction.exists(_.isDeployed)
    def isCanceled: Boolean = lastAction.exists(_.isCanceled)
  }

  object BaseProcessDetails {
    implicit def encoder[T](implicit shape: Encoder[T]): Encoder[BaseProcessDetails[T]] = deriveEncoder
    implicit def decoder[T](implicit shape: Decoder[T]): Decoder[BaseProcessDetails[T]] = deriveDecoder
  }

  case class BaseProcessDetails[ProcessShape](id: String, //TODO: replace it by Long / ProcessId
                                              name: String,
                                              processId: Long, //TODO: Remove it when we will support Long / ProcessId
                                              processVersionId: Long,
                                              isLatestVersion: Boolean,
                                              description: Option[String],
                                              isArchived: Boolean,
                                              isSubprocess: Boolean,
                                              processType: ProcessType,
                                              processingType: ProcessingType,
                                              processCategory: String,
                                              modificationDate: LocalDateTime,
                                              createdAt: LocalDateTime,
                                              createdBy: String,
                                              tags: List[String],
                                              lastDeployedAction: Option[ProcessAction],
                                              lastAction: Option[ProcessAction],
                                              json: Option[ProcessShape],
                                              history: List[ProcessVersion],
                                              modelVersion: Option[Int]) {

    def isDeployed: Boolean = lastAction.exists(_.isDeployed)
    def isCanceled: Boolean = lastAction.exists(_.isCanceled)
    def mapProcess[NewShape](action: ProcessShape => NewShape) : BaseProcessDetails[NewShape] = copy(json = json.map(action))
    // todo: unsafe toLong; we need it for now - we use this class for both backend (id == real id) and frontend (id == name) purposes
    def idWithName: ProcessIdWithName = ProcessIdWithName(ProcessId(processId), ProcessName(name))
  }

  // TODO we should split ProcessDetails and ProcessShape (json), than it won't be needed. Also BasicProcess won't be necessary than.
  sealed trait ProcessShapeFetchStrategy[ProcessShape]

  object ProcessShapeFetchStrategy {
    // TODO: Find places where FetchDisplayable is used and think about replacing it with FetchCanonical. We generally should use displayable representation
    //       only in GUI (not for deployment or exports) and in GUI it should be post processed using ProcessDictSubstitutor
    implicit case object FetchDisplayable extends ProcessShapeFetchStrategy[DisplayableProcess]
    implicit case object FetchCanonical extends ProcessShapeFetchStrategy[CanonicalProcess]
    // In fact Unit won't be returned inside shape and Nothing would be more verbose but it won't help in compilation because Nothing <: DisplayableProcess
    implicit case object NotFetch extends ProcessShapeFetchStrategy[Unit]
  }

  type ProcessDetails = BaseProcessDetails[DisplayableProcess]

  type ValidatedProcessDetails = BaseProcessDetails[ValidatedDisplayableProcess]

  @JsonCodec case class ProcessVersion(//processId: Long, //TODO: support it when will support processId as Long / ProcessId
                                       processVersionId: Long,
                                       createDate: LocalDateTime,
                                       user: String,
                                       modelVersion: Option[Int])

  @JsonCodec case class ProcessAction(//processId: Long, //TODO: support it when will support processId as Long / ProcessId
                                      processVersionId: Long,
                                      createdAt: LocalDateTime,
                                      user: String,
                                      action: ActionType,
                                      commentId: Option[Long],
                                      comment: Option[String],
                                      buildInfo: Map[String, String]) {
    def isDeployed: Boolean = action.equals(ActionType.Deploy)
    def isCanceled: Boolean = action.equals(ActionType.Cancel)
  }

  object ActionType extends Enumeration {
    implicit val typeEncoder: Encoder[ActionType.Value] = Encoder.enumEncoder(ActionType)
    implicit val typeDecoder: Decoder[ActionType.Value] = Decoder.enumDecoder(ActionType)

    type ActionType = Value
    val Deploy: Value = Value("DEPLOY")
    val Cancel: Value = Value("CANCEL")
  }
}
