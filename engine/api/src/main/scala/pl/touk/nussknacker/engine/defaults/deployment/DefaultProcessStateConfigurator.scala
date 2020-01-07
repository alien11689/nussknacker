package pl.touk.nussknacker.engine.defaults.deployment

import pl.touk.nussknacker.engine.api.deployment.StateAction.StateAction
import pl.touk.nussknacker.engine.api.deployment.StatusState.StateStatus
import pl.touk.nussknacker.engine.api.deployment.{ProcessStateConfigurator, StateAction}

object DefaultProcessStateConfigurator extends ProcessStateConfigurator {
  val defaultActions = List(StateAction.Deploy)

  val statusActions: Map[StateStatus, List[StateAction]] = Map(
    DefaultStateStatus.Unknown -> List(StateAction.Deploy),
    DefaultStateStatus.NotDeployed -> List(StateAction.Deploy),
    DefaultStateStatus.DuringDeploy -> List(StateAction.Cancel),
    DefaultStateStatus.Running -> List(StateAction.Cancel, StateAction.Pause),
    DefaultStateStatus.Canceled -> List(StateAction.Deploy),
    DefaultStateStatus.Restarting -> List(StateAction.Cancel),
    DefaultStateStatus.Failed -> List(StateAction.Deploy),
    DefaultStateStatus.Finished -> List(StateAction.Deploy)
  )

  override def statusTooltips: Map[StateStatus, String] = Map.empty

  override def statusIcons: Map[StateStatus, String] = Map.empty

  override def isFinished(stateStatus: StateStatus): Boolean =
    stateStatus == DefaultStateStatus.Finished

  override def isRunning(stateStatus: StateStatus): Boolean =
    stateStatus == DefaultStateStatus.Running

  override def isDuringDeploy(stateStatus: StateStatus): Boolean =
    stateStatus == DefaultStateStatus.DuringDeploy

  override def getStatusActions(status: StateStatus): List[StateAction] =
    statusActions.getOrElse(status, defaultActions)
}