package logger

import java.util.UUID
import java.util.concurrent.{ConcurrentHashMap, ConcurrentSkipListSet}

trait AuditLogger {
  def start(uuid: UUID, time: Long)

  def end(uuid: UUID, status: Int, time: Long)
}

case class Request(uuid: UUID, startTime: Long) {
  var endTime: Long = -1
  var statusCode: Int = 0

  def isComplete: Boolean = endTime != -1

  def complete(status: Int, time: Long): Unit = {
    endTime = time
    statusCode = status
  }

  def logString: Option[String] =
    if (isComplete)
      Some("id = %s status = %d duration = %d ms".format(uuid.toString, statusCode, endTime - startTime))
    else None
}

object SampleAuditLogger extends AuditLogger {

  private val queue = new ConcurrentSkipListSet[Request]((o1, o2) => math.signum(o1.startTime - o2.startTime).toInt)

  private val map = new ConcurrentHashMap[UUID, Request]()

  override def start(uuid: UUID, time: Long): Unit = {
    val request = Request(uuid, time)

    queue.add(request)
    map.put(uuid, request)
  }

  override def end(uuid: UUID, status: Int, time: Long): Unit = {
    Option(map.get()).foreach(_.complete(status, time))
    writeToFile()
  }

  private def writeToFile(): Unit = {

    if (queue.first().isComplete) {
      val request = queue.pollFirst()
      map.remove(request.uuid)

      // request.logString to file

      writeToFile()
    }
  }

  sys.addShutdownHook(shutDown())

  def shutDown(): Unit = {
    map.clear()
    queue.iterator().forEachRemaining(request => {
      if (request.isComplete) {
        // request.logString to file
      }
    })
  }

}
