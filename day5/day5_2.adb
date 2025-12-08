with Ada.Text_IO; use Ada.Text_IO;

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Containers.Vectors;

procedure Day5_2 is
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

   Total : Long_Integer := 0;
begin
   Open(File => Input_File, Mode => In_File, Name => "input.txt");
   
   Curr_Line := To_Unbounded_String(Get_Line(Input_File));
   RangeVector.Append (Ranges, ParseRange (To_String(Curr_Line)));
   Curr_Line := To_Unbounded_String(Get_Line(Input_File));

   declare
      NewRange : FreshRange;
   begin   
      while To_String(Curr_Line) /= "" loop
         NewRange := ParseRange (To_String(Curr_Line));

         for C in Ranges.First_Index..Ranges.Last_Index loop
            if NewRange.Upper = Ranges(C).Lower or NewRange.Upper = Ranges(C).Upper then
               NewRange.Upper := NewRange.Upper - Long_Integer(1);
            end if;

            if NewRange.Lower = Ranges(C).Upper or NewRange.Lower = Ranges(C).Lower then 
               NewRange.Lower := NewRange.Lower + Long_Integer(1);
            end if;

            if NewRange.Lower > NewRange.Upper then
               exit;
            end if;

            if NewRange.Lower < Ranges(C).Lower and NewRange.Upper > Ranges(C).Upper then
               Ranges(C).Upper := Ranges(C).Lower - Long_Integer(1);
            elsif NewRange.Lower <= Ranges(C).Upper then 
               if NewRange.Upper < Ranges(C).Lower then
                  Put_Line (NewRange.Lower'Image & " -" & NewRange.Upper'Image & " BEFORE" & Ranges(C).Lower'Image & " -" & Ranges(C).Upper'Image);
                  Ranges.Insert(C, NewRange);
                  exit;
               elsif NewRange.Upper < Ranges(C).Upper then
                  NewRange.Upper := Ranges(C).Lower - Long_Integer(1);
                  if NewRange.Lower <= NewRange.Upper then
                     Put_Line (NewRange.Lower'Image & " -" & NewRange.Upper'Image & " BEFORE" & Ranges(C).Lower'Image & " -" & Ranges(C).Upper'Image);
                     Ranges.Insert(C, NewRange);
                     exit;
                  end if;
               else
                  NewRange.Lower := Ranges(C).Upper + Long_Integer(1);
               end if;
            end if;

            if C = Ranges.Last_Index and NewRange.Lower <= NewRange.Upper then
               Put_Line ("APPEND: " & NewRange.Lower'Image & " -" & NewRange.Upper'Image);
               RangeVector.Append (Ranges, NewRange);
            end if;
         end loop;

         Curr_Line := To_Unbounded_String(Get_Line(Input_File));
      end loop;
   end;

   New_Line;
   -- count
   for RangeElement of Ranges loop
      Total := Total + (RangeElement.Upper - RangeElement.Lower + Long_Integer(1));
      Put_Line (RangeElement.Lower'Image & " -" & RangeElement.Upper'Image & ": " & Long_Integer'Image((RangeElement.Upper - RangeElement.Lower + 1)));
   end loop;   

   Put_Line ("Total:" & Total'Image);

end Day5_2;