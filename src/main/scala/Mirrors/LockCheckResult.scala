package Mirrors

case class LockCheckResult(defaultToLocked: Boolean, keys: List[(Int, Int)])
