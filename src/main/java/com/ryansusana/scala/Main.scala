package com.ryansusana.scala

import java.util.stream.Collectors

import com.google.cloud.functions.{HttpFunction, HttpRequest, HttpResponse}

class Main extends HttpFunction {
  def service(request: HttpRequest, response: HttpResponse): Unit = {

    // Request.getParts is a Map<String, Part>
    val filenames = request.getParts.entrySet()
      .stream() map { e => e.getKey } collect Collectors.joining(",")

    val writer = response.getWriter
    writer.write(s"$filenames")
  }
}
