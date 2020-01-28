package pl.touk.nussknacker.engine.api.deployment.simple

import io.circe.Json
import pl.touk.nussknacker.engine.api.ProcessVersion
import pl.touk.nussknacker.engine.api.deployment.StateStatus
import pl.touk.nussknacker.engine.api.deployment.{DeploymentId, ProcessState}

object SimpleProcessState {
  def apply(deploymentId: DeploymentId,
            status: StateStatus,
            processVersionId: Option[ProcessVersion] = Option.empty,
            startTime: Option[Long] = Option.empty,
            attributes: Option[Json] = Option.empty,
            errors: List[String] = List.empty): ProcessState =
    ProcessState(
      deploymentId = deploymentId,
      status = status,
      processVersionId = processVersionId,
      definitionManager = SimpleProcessStateDefinitionManager,
      startTime = startTime,
      attributes = attributes,
      errors = errors
    )
}
