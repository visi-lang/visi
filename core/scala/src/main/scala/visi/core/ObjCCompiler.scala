package visi.core

/**
 * Created with IntelliJ IDEA.
 * User: dpp
 * Date: 2/5/13
 * Time: 10:28 AM
 * To change this template use File | Settings | File Templates.
 */
object ObjCCompiler {

  def validSymbol(in: FuncName): String = {
    val str = in.name
    val len = str.length
    val ret = new StringBuffer(len + 10)

    var pos = 0
    while (pos < len) {
      str.charAt(pos) match {
        case c if c >= '0' && c <= '9' => ret.append(c)
        case c if c >= 'a' && c <= 'z' => ret.append(c)
        case c if c >= 'A' && c <= 'Z' => ret.append(c)
        case c => ret.append("_")
        ret.append(c.toInt.toString)
        ret.append("_")
       }
      pos += 1;
    }

    ret.toString
  }

  def buildSource(in: SourceExp): String = "- (IBAction)set_"+validSymbol(in.name)+":(id)sender;"
  def buildSink(in: SinkExp): String = "@property (weak, nonatomic) IBOutlet id "+validSymbol(in.name)+";"

  def hFile(name: String, sources: List[SourceExp], sinks: List[SinkExp]): String =
    ("""
      |//
      |//  This code was autogenerated. Do not edit it. Subclass it
      |//
      |
      |#import <Foundation/Foundation.h>
      |
      |@interface """+name+""" : UIViewController
      |
      |"""+
      sources.map(buildSource).mkString("\n")+
      """
      |
      |"""+
      sinks.map(buildSink).mkString("\n")+"""
      |
      |@property (weak, nonatomic) IBOutlet id webview;
      |
      |@end
      |
    """).stripMargin

  /**
   * Construct a ObjectiveC .m file based on the file/class name, the JavaScript, and the sources and sinks
   * @param name the name of the class
   * @param js the JavaScript that defines the Visi program
   * @param sources the list of sources
   * @param sinks the list of the sinks (currently unused)
   * @return a string that represents the objective-c program
   */
  def mFile(name: String, js: String, sources: List[SourceExp], sinks: List[SinkExp]): String = {


  ("""//
    |//  This code was autogenerated. Do not edit it. Subclass it
    |
    |#import """+('"'+name)+""".h"
    |#import <Foundation/Foundation.h>
    |
    |@implementation """+(name)+"""
    |
    |
    |- (void)viewDidLoad
    |{
    |    [super viewDidLoad];
    |
    |    NSString *prog = """+jsToStr(js)+"""
    |
    |    [self.webview stringByEvaluatingJavaScriptFromString: prog];
    |}
    |
    |
    """+sources.map(emitMethod).mkString("\n\n")+"""
    |
    |- (void) compute: (NSDictionary *)toRun {
    |    NSData* jsonData = [NSJSONSerialization dataWithJSONObject:toRun
    |                                                       options:NSJSONWritingPrettyPrinted error:nil];
    |    NSString *str = [[NSString alloc] initWithData:jsonData
    |                          encoding:NSUTF8StringEncoding];
    |    NSString *ret = [self.webview stringByEvaluatingJavaScriptFromString: [NSString stringWithFormat:@"$_exec_str(%@)", str]];
    |
    |    NSError *error;
    |    NSDictionary *jsonParsed = [[NSJSONSerialization JSONObjectWithData:[ret dataUsingEncoding:NSUTF8StringEncoding]
    |                              options:NSJSONReadingMutableContainers error:&error] objectForKey:@"success"];
    |    if (jsonParsed) {
    |    NSArray *keys = [jsonParsed allKeys];
    |    for (NSUInteger i = 0; i < [keys count]; i++) {
    |      NSString * key = [keys objectAtIndex: i];
    |      id value = [jsonParsed objectForKey: key];
    |      SEL sel = NSSelectorFromString(key);
    |      [[self performSelector:sel] setText: ([value respondsToSelector:@selector(stringValue)] ? [value stringValue] : value)];
    |
    |    } }
    |
    |}
    |
    |@end
    |""").stripMargin
  }

  private def emitMethod(in: SourceExp): String = {
    def action: String = in.tpe match {
      case TPrim(PrimDouble) => "id value = [[NSNumber alloc] initWithDouble:[[sender text] doubleValue]];\n"
      case TPrim(PrimStr) => "id value = [sender text];\n"
      case TPrim(PrimBool) => "id value = [[NSNumber alloc] initWithBool:[sender state]];\n"

      case tpe =>
        println("The type is "+tpe+" for "+in)
        sys.error("I hate yaks")
        "id value = @\"error\";\n"
    }


    ("""- (IBAction)set_"""+validSymbol(in.name)+""":(id)sender {
      """+action+"""
      |    NSMutableDictionary *dict = [NSMutableDictionary dictionary];
      |    [dict setObject:value forKey:@"""+('"' + in.name.name + '"')+"""];
      |    [self compute: dict];
      |}
      |""").stripMargin
  }

  private def jsToStr(in: String): String = {
    val ret = new StringBuffer(in.length * 3)

    val strList: List[String] = (Compiler.jsLibrary + "\n\n" + in+"\n\n").split("\n").toList

    ret.append("@")
    strList.foreach(s => {
      ret.append("\"")
      ret.append(fixStr(s))
      ret.append("\\n\"\n")

    })
    ret.append(";\n\n")

    ret.toString()
  }

  private def fixStr(in: String): String = {
    val ret = new StringBuffer(in.length + 20)

    val len = in.length
    var pos = 0
    while (pos < len) {
      in.charAt(pos) match {
        case '"' => ret.append("\\\"")
        case '\\' => ret.append("\\\\")
        case c => ret.append(c)
      }
      pos += 1
    }

    ret.toString
  }
}
