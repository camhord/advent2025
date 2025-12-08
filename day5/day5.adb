with Ada.Text_IO; use Ada.Text_IO;

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Containers.Vectors;

procedure Day5 is
   type FreshRange is record
      Lower, Upper : Long_Integer := 0;
   end record;

   package RangeVector is new Ada.Containers.Vectors (Natural, FreshRange);
   use RangeVector;
   Ranges : Vector;

   function ParseRange (Line : String) return FreshRange is 
   begin
      for I in Line'Range loop
         if Line(I) = '-' then
            return ( Long_Integer'Value(Line(Line'First..(I - 1))), 
                     Long_Integer'Value(Line((I + 1)..Line'Last)) );
         end if;
      end loop;

      return (0,0);
   end ParseRange;

   function TestValue(Value: Long_Integer) return Boolean is
   begin
      for VRange of Ranges loop
         if Value >= VRange.Lower and Value <= VRange.Upper then
            return true;
         end if;
      end loop;

      return false;
   end TestValue;

   Input_File : File_Type;

   Curr_Line : Unbounded_String;

   Total : Natural := 0;
begin
   Open(File => Input_File, Mode => In_File, Name => "input.txt");
   
   Put_Line("Load Ranges");
   -- load ranges
   Curr_Line := To_Unbounded_String(Get_Line(Input_File));
   while To_String(Curr_Line) /= "" loop
      RangeVector.Append(Ranges, ParseRange (To_String(Curr_Line)));
      Curr_Line := To_Unbounded_String(Get_Line(Input_File));
   end loop;

   Put_Line ("Test values");
   -- test values
   while not End_Of_File(Input_File) loop
      Curr_Line := To_Unbounded_String(Get_Line(Input_File));
      Put (To_String(Curr_Line));

      if TestValue (Long_Integer'Value(To_String(Curr_Line))) then
         Total := Total + 1;
         Put(" - GOOD!");
      else 
         Put (" - BAD!");
      end if;

      New_Line;
   end loop;

   Put_Line ("Total:" & Total'Image);

end Day5;