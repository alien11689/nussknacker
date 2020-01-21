package pl.touk.nussknacker.ui.db.entity

import java.sql.Timestamp
import java.time.LocalDateTime

import pl.touk.nussknacker.restmodel.processdetails.ActionType
import pl.touk.nussknacker.restmodel.processdetails.ActionType.ActionType
import pl.touk.nussknacker.ui.util.DateUtils
import slick.ast.BaseTypedType
import slick.jdbc.{JdbcProfile, JdbcType}
import slick.lifted.{ForeignKeyQuery, ProvenShape, TableQuery => LTableQuery}
import slick.sql.SqlProfile.ColumnOption.{NotNull, Nullable}

trait ProcessActionEntityFactory {

  protected val profile: JdbcProfile
  import profile.api._

  implicit def deploymentMapper: JdbcType[ActionType] with BaseTypedType[ActionType] =
    MappedColumnType.base[ActionType, String](_.toString, ActionType.withName)

  val processActionsTable: LTableQuery[ProcessActionEntityFactory#ProcessActionEntity] =
    LTableQuery(new ProcessActionEntity(_))

  val processVersionsTable: LTableQuery[ProcessVersionEntityFactory#ProcessVersionEntity]
  val commentsTable: LTableQuery[CommentEntityFactory#CommentEntity]
  val environmentsTable: LTableQuery[EnvironmentsEntityFactory#EnvironmentsEntity]

  class ProcessActionEntity(tag: Tag) extends Table[ProcessActionEntityData](tag, "process_actions") {
    def processId: Rep[Long] = column[Long]("process_id", NotNull)

    def processVersionId: Rep[Long] = column[Long]("process_version_id", Nullable)

    def createdAt: Rep[Timestamp] = column[Timestamp]("created_at", NotNull)

    def user: Rep[String] = column[String]("user", NotNull)

    def buildInfo: Rep[Option[String]] = column[Option[String]]("build_info", Nullable)

    def action: Rep[ActionType] = column[ActionType]("action", NotNull)

    def commentId: Rep[Option[Long]] = column[Option[Long]]("comment_id", Nullable)

    def pk = primaryKey("process_actions_pk", (processId, processVersionId, createdAt))

    def processes_fk: ForeignKeyQuery[ProcessVersionEntityFactory#ProcessVersionEntity, ProcessVersionEntityData] = foreignKey("process_actions_version_fk", (processId, processVersionId), processVersionsTable)(
      procV => (procV.processId, procV.id),
      onUpdate = ForeignKeyAction.Cascade,
      onDelete = ForeignKeyAction.NoAction
    )

    def comment_fk: ForeignKeyQuery[CommentEntityFactory#CommentEntity, CommentEntityData] = foreignKey("process_actions_comment_fk", commentId, commentsTable)(
      _.id.?,
      onUpdate = ForeignKeyAction.Cascade,
      onDelete = ForeignKeyAction.SetNull
    )

    def * : ProvenShape[ProcessActionEntityData] = (processId, processVersionId, user, createdAt, action, commentId, buildInfo) <> (
      ProcessActionEntityData.tupled, ProcessActionEntityData.unapply
    )
  }
}

case class ProcessActionEntityData(processId: Long,
                                   processVersionId: Long,
                                   user: String,
                                   createdAt: Timestamp,
                                   action: ActionType,
                                   commentId: Option[Long],
                                   buildInfo: Option[String]) {

  lazy val deployedAtTime: LocalDateTime = DateUtils.toLocalDateTime(createdAt)
  lazy val isDeployed: Boolean = action.equals(ActionType.Deploy)
  lazy val isCanceled: Boolean = action.equals(ActionType.Cancel)
}