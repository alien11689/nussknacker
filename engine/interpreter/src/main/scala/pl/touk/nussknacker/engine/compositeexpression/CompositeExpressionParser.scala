package pl.touk.nussknacker.engine.compositeexpression

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import pl.touk.nussknacker.engine.api.{CirceUtil, Context}
import pl.touk.nussknacker.engine.api.context.ValidationContext
import pl.touk.nussknacker.engine.api.expression.{Expression, ExpressionParseError, ExpressionParser, ExpressionTypingInfo, TypedExpression, ValueWithLazyContext}
import pl.touk.nussknacker.engine.api.lazyy.LazyValuesProvider
import pl.touk.nussknacker.engine.api.typed.typing
import pl.touk.nussknacker.engine.graph.expression
import pl.touk.nussknacker.engine.util.validated.ValidatedSyntax
import cats.instances.list._
import pl.touk.nussknacker.engine.api.typed.typing.TypedObjectTypingResult

//FIXME: should not be here
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future

class CompositeExpressionParser(parsers: Map[String, ExpressionParser]) extends ExpressionParser {

  def this(parsers: Seq[ExpressionParser]) = this(parsers.map(k => k.languageId -> k).toMap)

  private val syntax = ValidatedSyntax[ExpressionParseError]
  import syntax._

  override def languageId: String = "composite"

  override def parse(original: String, ctx: ValidationContext, expectedType: typing.TypingResult)
    : ValidatedNel[ExpressionParseError, TypedExpression] = parse(original).andThen { parsed =>
    parsed.map {
      case (k, v) => parser(v).andThen(_.parse(v.expression, ctx, expectedType)).map(k -> _)
    }.toList.sequence.map(_.toMap).map { expressionMap =>
      val expression = CompositeExpression(original, expressionMap.mapValues(_.expression))
      val types = TypedObjectTypingResult(expressionMap.mapValues(_.returnType))
      TypedExpression(expression, types, CompositeExpressionTypingInfo)
    }
  }

  private def parser(expressionToParse: expression.Expression): Validated[NonEmptyList[ExpressionParseError], ExpressionParser]
    = parsers.get(expressionToParse.language).map(Valid(_)).getOrElse(Invalid(NonEmptyList.of(ExpressionParseError(s"Failed to find language: ${expressionToParse.language}"))))

  override def parseWithoutContextValidation(original: String)
  : ValidatedNel[ExpressionParseError, Expression] = parse(original).andThen { parsed =>
    parsed.map {
      case (k, v) => parser(v).andThen(_.parseWithoutContextValidation(v.expression)).map(k -> _)
    }.toList.sequence.map(_.toMap).map(CompositeExpression(original, _))
  }

  private def parse(value: String): Validated[NonEmptyList[ExpressionParseError], Map[String, expression.Expression]] = Validated.fromEither(CirceUtil
    .decodeJson[Map[String, expression.Expression]](value)
    .left.map(err => NonEmptyList.of(ExpressionParseError(s"Failed to parse: ${err.getMessage}"))))

}

case object CompositeExpressionTypingInfo extends ExpressionTypingInfo

case class CompositeExpression(original: String, expressions: Map[String, Expression]) extends Expression {

  override def language: String = "composite"

  override def evaluate[T](ctx: Context, lazyValuesProvider: LazyValuesProvider): Future[ValueWithLazyContext[T]] = {
    Future.sequence(expressions.map {
      case (k, v) => v.evaluate[Any](ctx, lazyValuesProvider).map(res => k -> res.value)
    }).map(res => ValueWithLazyContext(res.asInstanceOf[T], ctx.lazyContext))
  }

}