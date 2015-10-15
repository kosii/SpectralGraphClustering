package controllers

import breeze.linalg.eigSym.EigSym
import breeze.linalg.functions.euclideanDistance
import play.api.mvc._
import play.api.libs.json._

class Application extends Controller {

  def index = Action {
    Ok(views.html.index())
  }

  import play.api.mvc._
  import play.api.Play.current

  def socket = WebSocket.acceptWithActor[JsValue, JsValue] { request => out =>
    MyWebSocketActor.props(out)
  }

  import akka.actor._

  object MyWebSocketActor {
    def props(out: ActorRef) = Props(new MyWebSocketActor(out))
  }

  case class Node(index: Int)
  object Node {
    implicit val format = Json.format[Node]
  }
  case class Link(source: Node, target: Node)
  object Link {
    implicit val format = Json.format[Link]
  }
  case class Update(node: Node, links: List[Link])
  object Update {
    implicit val format = Json.format[Update]
  }

  class MyWebSocketActor(out: ActorRef) extends Actor {
    import breeze.linalg._
    def getLaplacian(nodes: Set[Int], links: Set[(Int, Int)]): DenseMatrix[Double] = {
      val m = DenseMatrix.zeros[Double](nodes.size, nodes.size)
      for {
        (from, to) <- links
      } {
        m.update(from, to, -1)
        m.update(to, from, -1)
        m.update(to, to, m.valueAt(to, to) + 1)
        m.update(from, from, m.valueAt(from, from) + 1)
      }
      m
    }

    def getColors(laplacian: DenseMatrix[Double]): DenseVector[(Double, Double, Double)] = {

      val EigSym(values, vectors) = eigSym(laplacian)

      if (vectors.cols > 3) {
        vectors(::, 1).mapPairs({ case (i, d) => (d, vectors(i, 2), vectors(i, 3)) })
      } else if (vectors.cols > 2) {
        vectors(::, 1).mapPairs({ case (i, d) => (d, vectors(i, 2), 0.0) })
      } else if (vectors.cols > 1) {
        vectors(::, 1).mapPairs({ case (i, d) => (d, 0.0, 0.0) })
      } else {
        vectors(::, 0).map(d => (d, 0.0, 0.0))
      }
    }

    def receive = receive(Set(0), Set())
    def receive(nodes: Set[Int], links: Set[(Int, Int)]): Receive = {
      case msg: JsValue => Json.fromJson[Update](msg) match {
        case JsError(_) =>
          println("ERROR")
        case JsSuccess(update, _) =>
          val newNodes = nodes + update.node.index
          val newLinks = links ++ update.links.map({ case Link(from, to) =>
            (from.index, to.index)
          })
          context.become(receive(newNodes, newLinks))
          val colors = getColors(getLaplacian(newNodes, newLinks))
          println(colors.toArray.toList.map(d => Map("color" -> d)))
          import nak.cluster._
          val vectorizedResults = colors.toArray.map({case (a, b, c) => Vector(a, b, c)})
          val kmeans = new Kmeans[Vector[Double]](vectorizedResults)
          val (_, centroids) = kmeans.run(4, 10)
          val (_, memberships) = kmeans.computeClusterMemberships(centroids)
          println(s"memberships: $memberships")
          val replacedWithCentroids = vectorizedResults.zip(memberships).map({case (a, b) => centroids(b)})
          println(s"centroids: $centroids")
          val transformed = replacedWithCentroids.map(d => Map("r" -> d(0), "g" -> d(1), "b" -> d(2)))
          println(s"transformed result of kmeans: $replacedWithCentroids")
//          println(update)
          out ! Json.toJson(transformed)
      }
    }
  }
}
