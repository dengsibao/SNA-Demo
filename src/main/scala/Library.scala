/**
 * author: dengsibao
 * Date： 2014-12-11.
 */

import com.sun.jna.{Function => JNAFunction}
import scala.language.dynamics
import scala.collection.mutable

class Library(val libName: String) extends Dynamic {

  class Invocation(val jnaFunction: JNAFunction,val args: Array[AnyRef]){
    def apply[R](implicit m: Manifest[R]): R = {
      if(m.runtimeClass == classOf[Unit]) {
        jnaFunction.invoke(args).asInstanceOf[R]
      }else {
        jnaFunction.invoke(m.runtimeClass,args).asInstanceOf[R]
      }
    }

    def as[R](implicit m: Manifest[R]) = apply[R](m)

    def asInstanceOf[R](implicit m: Manifest[R]) = apply[R](m)
  }

  def applyDynamic(functionName: String)(args: Any*): Invocation = {
    var jnaFunction: JNAFunction = null
    if(functionCache.contains(functionName)){
      jnaFunction = functionCache(functionName)
    }else{
      jnaFunction = JNAFunction.getFunction(libName,functionName)
      functionCache(functionName) = jnaFunction
    }
    new Invocation(jnaFunction,args.map(_.asInstanceOf[AnyRef]).toArray[AnyRef])
  }

  private val functionCache = mutable.Map.empty[String,JNAFunction]
}

object Library extends  App{

  def apply(libName: String) = new Library(libName)

  val myLib = Library("msvcrt")

  println(myLib.atol("0345")[Int])
  println(myLib.malloc(10000)[Int])
  myLib.printf("[printf] clock: %ld\n",myLib.clock().asInstanceOf[Int]).as[Unit]
  myLib.printf("value: %d %f", 123,3.12).as[Unit]
}
