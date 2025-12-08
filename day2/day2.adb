with Ada.Text_IO; use Ada.Text_IO;
with Ada.Sequential_IO;

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Day2 is
  function CheckSubstring (Value : String; Sub : String) return Boolean is
    Length : Natural := Value'Length;
    SubLen : Natural := Sub'Length;
  begin
    if Sub(Sub'Last) /= Value(Value'Last) then
      --Put_Line(Sub & ": " & Sub(Sub'Last) & "/=" & Value & ": " & Value(Value'Last));
      return false;
    elsif (Value'Length / Sub'Length) * Sub'Length /= Value'Length then
      --Put_Line(Sub & " no mod " & Value);
      return false;
    elsif Sub'Length = Value'Length then
      return false;
    end if;

    for Index in Value'Range loop
      if Sub((Index - Value'First) mod SubLen + Sub'First) /= Value(Index) then
         return false;
      end if;
    end loop;
    Put_Line(Sub & " - " & Value);

    return true;

  end CheckSubstring;

  function CheckDupe (Value : String) return Boolean is 
  begin
   --return Value(Value'First..(Value'Length / 2)) = Value((Value'Length / 2 + 1)..Value'Last);

    for I in Value'First .. (Value'Last / 2 + 1) loop
      if CheckSubstring(Value, Value(Value'First..I)) then
        return true;
      end if;
    end loop;

    return false;
  end CheckDupe;

  package CharIO is new Ada.Sequential_IO (Character);
  use CharIO;
  F : CharIO.File_Type;
  File_Name : constant String := "input.txt";
  
  C : Character;

  ParseBuffer : Unbounded_String := Null_Unbounded_String;

  LowerBound, UpperBound : Long_Integer;

  Total : Long_Integer := 0;
begin
  Open(F, In_File, File_Name);

  loop
    CharIO.Read(F, C);

    if C = '-' then
      LowerBound := Long_Integer'Value(To_String(ParseBuffer));
      ParseBuffer := Null_Unbounded_String;
    elsif C = ',' or End_Of_File (F) then
      UpperBound := Long_Integer'Value(To_String(ParseBuffer));
      ParseBuffer := Null_Unbounded_String;

      for Value in LowerBound .. UpperBound loop
        if CheckDupe(Trim (Source => Value'Image, Side => Ada.Strings.Both)) then
          Put_Line(Value'Image);
          Total := Total + Value;
        end if;
      end loop;
    else 
      ParseBuffer := ParseBuffer & C;
    end if;

    if End_Of_File (F) then
      exit;
    end if;
  end loop;

  Put_Line("Total: " & Total'Image);

  Close(F);
end Day2;
