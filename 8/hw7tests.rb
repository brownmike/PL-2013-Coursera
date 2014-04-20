# University of Washington, Programming Languages, Homework 7,
# hw7tests.rb
 
require 'simplecov'
SimpleCov.command_name "My Tests"
SimpleCov.start
 
require "./hw7testsprovided.rb"
 
class Object
  def eq? other
    # one way to do "deep equality".
    # irb prints e1.inspect (a string) that's easier to read than Marshal.dump
    # However, that includes a hash of the object_id so can't be used to compare
    Marshal.dump(self) == Marshal.dump(other)
  end
  def ev
    preprocess_prog.eval_prog([])
  end
end
 
def test(e1,e2,s)
  puts(e1.ev.eq?(e2.ev) ? s : s+": NOK")
end
 
# preprocess
test(LineSegment.new(1.0,2.0,1.0,2.0),
     Point.new(1.0,2.0), "preprocess1")
test(LineSegment.new(1.0,2.0,1.000001,-2.0),
     LineSegment.new(1.000001,-2.0,1.0,2.0),"preprocess2")
 
# intersect
test(Intersect.new(Line.new(1.0,0.0),LineSegment.new(1.0,1.0,-1.0,-1.0)),
     LineSegment.new(-1.0,-1.0,1.0,1.0), "intersect1");
test(Intersect.new(Point.new(1.0,1.0),Line.new(1.0,0.0)),
     Point.new(1.0,1.0), "intersect2");
test(Intersect.new(LineSegment.new(0.5,0.5,0.0,0.0),LineSegment.new(1.0,1.0,-1.0,-1.0)),
     LineSegment.new(0.0,0.0,0.5,0.5), "intersect3");
test(Intersect.new(VerticalLine.new(0.0),VerticalLine.new(1.0)),
     NoPoints.new, "intersect4");
test(Intersect.new(VerticalLine.new(0.0),LineSegment.new(0.0,1.0,0.0,2.0)),
     LineSegment.new(0.0,1.0,0.0,2.0), "intersect5");
test(Intersect.new(VerticalLine.new(0.0),NoPoints.new),
     NoPoints.new, "intersect5");
test(Intersect.new(Line.new(1.0,0.0),NoPoints.new),
     NoPoints.new, "intersect6");
test(Intersect.new(Line.new(1.0,0.0),Point.new(2.0,2.0)),
     Point.new(2.0,2.0), "intersect7");
test(Intersect.new(Line.new(1.0,0.0),Point.new(2.0,1.0)),
     NoPoints.new, "intersect8");
test(Intersect.new(VerticalLine.new(0.0),Point.new(-0.00000001,1.0)),
     Point.new(-0.00000001,1.0), "intersect9");
test(Intersect.new(Point.new(2.0,1.0),VerticalLine.new(0.0)),
     NoPoints.new, "intersect10");
test(Intersect.new(Line.new(1.0,0.0),VerticalLine.new(2.0)),
     Point.new(2.0,2.0), "intersect11");
test(Intersect.new(Point.new(2.0,1.0),NoPoints.new),
     NoPoints.new, "intersect12");
test(Intersect.new(NoPoints.new,NoPoints.new),
     NoPoints.new, "intersect13");
test(Intersect.new(Point.new(0.0,1.5),LineSegment.new(0.0,1.0,0.0,2.0)),
     Point.new(0.0,1.5), "intersect14");
test(Intersect.new(Point.new(0.0,3),LineSegment.new(0.0,1.0,0.0,2.0)),
     NoPoints.new, "intersect15");
test(Intersect.new(LineSegment.new(0.0,0.0,0.0,3.0),LineSegment.new(0.0,1.0,0.0,2.0)),
     LineSegment.new(0.0,1.0,0.0,2.0), "intersect16");
test(Intersect.new(LineSegment.new(0.0,3.0,0.0,4.0),LineSegment.new(0.0,1.0,0.0,2.0)),
     NoPoints.new, "intersect17");
test(Intersect.new(LineSegment.new(0.0,1.5,0.0,2.5),LineSegment.new(0.0,1.0,0.0,2.0)),
     LineSegment.new(0.0,1.5,0.0,2.0), "intersect18");
test(Intersect.new(LineSegment.new(0.0,3.0,0.0,2.0),LineSegment.new(0.0,1.0,0.0,2.0)),
     Point.new(0.0,2.0), "intersect19");
 
# shift
test(Shift.new(1.0,2.0,NoPoints.new),
     NoPoints.new, "shift1");
 
# let
test(Let.new("x",LineSegment.new(0.5,0.5,0.0,0.0),
             Let.new("y",Shift.new(1.0,1.0,Var.new("x")),
                     Var.new("y"))),
     LineSegment.new(1.0,1.0,1.5,1.5), "let1");
 
test(Let.new("x",VerticalLine.new(0.2),
             Intersect.new(Shift.new(0.3,1.0,Var.new("x")),
                           LineSegment.new(1.0,1.0,-1.0,-1.0))),
     Point.new(0.5,0.5), "let2");
 
test(Let.new("x",VerticalLine.new(0.5),
             Let.new("y",LineSegment.new(1.0,1.0,-1.0,-1.0),
                     Intersect.new(Var.new("x"),Var.new("y")))),
     Point.new(0.5,0.5), "let3");
 
# context 'preprocess' do
#   it 'should be equal' do
#     be_eq
#   end
# end