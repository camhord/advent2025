with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Containers.Vectors;

procedure Day6 is 
  File_Name : constant String := "input.txt";
  Input_File : File_Type;
  Whitespace : constant Character_Set := To_Set (' ');

  type Line is record
    Buffer : Unbounded_String;
    Position : Natural := 0;
    Is_End : Boolean := false;
  end record;
  package LinesVector is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Line);
  Lines : LinesVector.Vector;

  function ParseNext (L : in out Line) return String is 
    First : Positive;
    Last : Natural;
  begin
    Find_Token(L.Buffer, Whitespace, L.Position, Outside, First, Last);
    L.Position := Last + 1;

    if Last = 0 then 
      L.Is_End := true;
      return "";
    end if;

    return To_String(L.Buffer)(First..Last);
  end ParseNext;

  function CalculateNext return Long_Integer is
    Total : Long_Integer := 0;
    Temp : Long_Integer;
    Operator : String := ParseNext(Lines(Lines.Last_Index));
  begin
   if Operator = "+" then 
      Total := 0;
   elsif Operator = "*" then
      Total := 1;
   else 
      Put_Line("Unknown operator: " & Operator);
      return 0;
   end if;

   for I in Lines.First_Index..Lines.Last_Index - 1 loop
      Temp := Long_Integer'Value(ParseNext(Lines(I)));

      Put(Temp'Image & " ");
      if I < Lines.Last_Index - 1 then
         Put(Operator & " ");
      end if;

      if Operator = "*" then
         Total := Total * Temp;
      elsif Operator = "+" then
         Total := Total + Temp;
      end if;
   end loop;

   Put_Line(" = " & Total'Image);
   return Total;
  end CalculateNext;

  Final : Long_Integer := 0;

begin
  Open(Input_File, In_File, File_Name);

  while not End_Of_File(Input_File) loop
    Lines.Append(New_Item => (Buffer => To_Unbounded_String(Get_Line(Input_File)), Position => 1, Is_End => false));
  end loop;

   while Lines(Lines.Last_Index).Is_End = false loop
      Final := Final + CalculateNext;
   end loop;

   Put_Line("Final: " & Final'Image);

end Day6;
