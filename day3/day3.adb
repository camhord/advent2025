with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure Day3 is
   Input_File : File_Type;

   Total : Long_Integer := 0;
begin
   Open(File => Input_File, Mode => In_File, Name => "input.txt");

   while not End_Of_File(Input_File) loop
      declare
         Line_Content : String := Get_Line(Input_File);

         TempVal : Integer;

         S_Value : String (1..12);

         I_Max : Integer;
         V_Max : Integer := 0;
      begin
         Put_Line (Line_Content);
         I_Max := Line_Content'First - 1;
         for V_Index in S_Value'Range loop
            V_Max := 0;
            for I in (I_Max + 1)..(Line_Content'Last - 11 + (V_Index - S_Value'First)) loop
               TempVal := Integer'Value((1 => Line_Content(I)));
               if TempVal > V_Max then
                  V_Max := TempVal;
                  I_Max := I;
               end if;
            end loop;
            
            S_Value(V_Index) := Line_Content(I_Max);
         end loop;

         Total := Total + Long_Integer'Value(S_Value);

         Put_Line (S_Value);
      end;

   end loop;

   Put_Line ("  = " & Total'Image);

   Close(Input_File);
end Day3;