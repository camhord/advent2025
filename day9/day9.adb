with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Containers.Vectors;

procedure Day9 is
   type Point is record
      X, Y : Long_Integer;
   end record;
   package PointVector is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Point);
   Points : PointVector.Vector;

   package IntegerVector is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Long_Integer);
   package IntVecSort is new IntegerVector.Generic_Sorting;
   Areas : IntegerVector.Vector;

   function To_String(N : Point) return String is 
   begin
      return "(" & N.X'Image & "," & N.Y'Image & ")"; 
   end To_String;

   function ParseNode (Point_String : String) return Point is 
      To_Return : Point;
      First : Positive := Point_String'First;
      Last : Natural;
   begin
      Last := Ada.Strings.Fixed.Index(Point_String(First..Point_String'Last), ",");
      To_Return.X := Long_Integer'Value(Point_String(First..Last - 1));

      To_Return.Y := Long_Integer'Value(Point_String(Last + 1..Point_String'Last));

      return To_Return;
   end ParseNode;

   function Distance(L, R: Point) return Float is 
   begin
      return Sqrt(Float(Float(L.X - R.X) ** 2 + Float(L.Y - R.Y) ** 2));
   end Distance;

   function Area(L, R: Point) return Long_Integer is 
   begin
      return (Abs(L.X - R.X) + 1) * (Abs(L.Y - R.Y) + 1);
   end Area;

   Input_File : File_Type;
   File_Name : String := "input.txt";
begin
   Open(Input_File, In_File, File_Name);

   while not End_Of_File(Input_File) loop
      Points.Append(New_Item => ParseNode(Get_Line(Input_File)));
   end loop;

   for I in Points.First_Index..Points.Last_Index loop
      for J in I + 1..Points.Last_Index loop
         Areas.Append(New_Item => Area(Points(I), Points(J)));
      end loop;
   end loop;

   IntVecSort.Sort (Areas);

   for I in Areas.First_Index..Areas.Last_Index loop
      Put_Line (Long_Integer'Image(Areas(I)));
   end loop;
end Day9;