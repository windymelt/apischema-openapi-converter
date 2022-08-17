package windymelt.a2o.apischema

case class Endpoint(
  method: String,
  path: String,
  title: String,
  description: String,
  destination: Destination,
  request: String,
  response: Response
)

case class Destination(controller: String, action: String, options: Seq[String])

case class RequestResource(parameter: Option[ResourceLike], body: Option[ResourceLike])

sealed trait ResourceLike
case class ResourceDesignator(id: String) extends ResourceLike

sealed trait Response
case class StatusCodeDictResponse(map: Map[Int, ResponseResource]) extends Response
case class StatusCodeAgnosticResponse(resource: ResponseResource, encoding: Option[String])

case class ResponseResource(encoding: Option[String], body: Option[ResourceLike])
