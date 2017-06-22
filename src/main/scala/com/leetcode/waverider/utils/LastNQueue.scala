package com.leetcode.waverider.utils

import scala.collection.mutable

class LastNQueue[T](val limit:Int) extends mutable.Queue[T] {

  override def enqueue(elems: T*): Unit = {
    this ++= elems

    while (size > limit) {super.dequeue()}
  }
}