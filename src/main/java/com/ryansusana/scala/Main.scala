package com.ryansusana.scala

import java.util.stream.Collectors

import com.google.cloud.functions.{HttpFunction, HttpRequest, HttpResponse}

class Main extends HttpFunction {
  def service(request: HttpRequest, response: HttpResponse): Unit = {

    // Request.getParts is a Map<String, Part>

    val contentType = request.getContentType.orElse("application/json")
    val filenames =
      if (contentType contains "multipart")
        request.getParts.entrySet().stream()
          .map { e => e.getKey } collect Collectors.joining(",")
      else
        "none"

    val writer = response.getWriter
    writer.write(s"Files: $filenames")
  }
}
