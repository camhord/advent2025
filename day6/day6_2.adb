with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Containers.Vectors;

procedure Day6_2 is 
  File_Name : constant String := "input.txt";
  Input_File : File_Type;
  Whitespace : constant Character_Set := To_Set (' ');

  type Line is record
    Buffer : Unbounded_String;
    Position : Natural := 0;
    Is_End : Boolean := false;
  end record;
  package LinesVector is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Line);
  Lines : LinesVector.Vector;

  function ParseNext (Ln : in out Line; F : out Natural; L : out Natural) return String is 
    First : Positive;
    Last : Natural;
    NextFirst : Positive;
    NextLast : Natural;
   begin
      Find_Token(Ln.Buffer, Whitespace, Ln.Position, Outside, First, Last);
      Ln.Position := Last + 1;
      F := First;

      if Last = 0 then 
         Ln.Is_End := true;
         return "";
      end if;

      Find_Token(Ln.Buffer, Whitespace, Ln.Position, Outside, NextFirst, NextLast);
      if NextLast = 0 then
         L := Length(Ln.Buffer);
      else
         L := NextFirst - 2;
      end if;

      return To_String(Ln.Buffer)(First..Last);
   end ParseNext;

  function CalculateNext return Long_Integer is
    Total : Long_Integer := 0;
    Temp : Long_Integer;
    First : Positive;
    Last : Natural;
    Operator : String := ParseNext(Lines(Lines.Last_Index), First, Last);
  begin
   if Operator = "+" then 
      Total := 0;
   elsif Operator = "*" then
      Total := 1;
   else 
      Put_Line("Unknown operator: " & Operator);
      return 0;
   end if;

   for I in First..Last loop
      declare
         NumberString : String (Lines.First_Index..Lines.Last_Index - 1);
      begin
         for Ln in Lines.First_Index..Lines.Last_Index - 1 loop
            NumberString(Ln) := Element(Lines(Ln).Buffer,I);
         end loop;
         Temp := Long_Integer'Value(NumberString);
      end;

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

end Day6_2;
