package com.ryansusana.scala

import com.google.cloud.functions.{HttpFunction, HttpRequest, HttpResponse}

class Main extends HttpFunction {
  def service(request: HttpRequest, response: HttpResponse): Unit = {

    val writer = response.getWriter
    writer.write("Hello World, from Ryan!")
  }
}
