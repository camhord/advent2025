with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;

with Ada.Directories; use Ada.Directories;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

procedure Day4_2 is
   File_Name : constant String := "input.txt";
   Roll : constant Character := '@';
   Empty : constant Character := '.';
   Thresh : constant Integer := 4;

   Input_File : File_Type;

   Total : Integer := 0;

   Line_Count : Natural := 0;
   Line_Length : Natural := 0;

   procedure CheckIndex (Line : Unbounded_String; Index : Integer; Count : in out Integer) is
   begin 
       if Element(Line, Integer(Index)) = Roll then
         Count := Count + 1;
      end if;
   end CheckIndex;
begin
   declare
      Temp : Unbounded_String;
   begin
      Open(File => Input_File, Mode => In_File, Name => File_Name);

      while not End_Of_File(Input_File) loop
         Line_Count := Line_Count + 1;
         Temp := To_Unbounded_String(Get_Line(Input_File));
         Line_Length := To_String(Temp)'Length;
      end loop;

      Close(Input_File);
   end;

   declare
      type CharMatrix is array(1..Line_Count, 1..Line_Length) of Character;
      ProblemSet : CharMatrix;

      Curr_Line : Unbounded_String;

      Row : Positive := 1;

      LocalCount : Integer := 0;

      procedure CheckCell (Row : Positive; Col : Positive; Count : in out Natural) is
      begin
         Count := (if ProblemSet(Row, Col) = Roll then Count + 1 else Count);
      end CheckCell;

      function CanRemove(Row : Positive; Col : Positive) return Boolean is
         Count : Natural := 0;
      begin
         if ProblemSet(Row, Col) = Empty then
            return false;
         end if;

         -- Check behind
         if Col > 1 then
            CheckCell(Row, Col - 1, Count);
         end if;

         -- Check ahead
         if Col < Line_Length then
            CheckCell(Row, Col + 1, Count);
         end if;

         -- check above line
         if Row > 1 then
            CheckCell(Row - 1, Col, Count);
            if Col > 1 then
               CheckCell(Row - 1, Col - 1, Count);
            end if;
            if Col < Line_Length then
               CheckCell(Row - 1, Col + 1, Count);
            end if;
         end if;

         -- check below line
         if Row < Line_Count then
            CheckCell(Row + 1, Col, Count);
            if Col > 1 then
               CheckCell(Row + 1, Col - 1, Count);
            end if;
            if Col < Line_Length then
               CheckCell(Row + 1, Col + 1, Count);
            end if;
         end if;

         return Count < Thresh;
      end CanRemove;

   begin
      Open(File => Input_File, Mode => In_File, Name => File_Name);

      -- load data into array
      while not End_Of_File(Input_File) loop
         declare
            Curr_Line : String := Get_Line(Input_File);
         begin
            for I in Curr_Line'Range loop
               ProblemSet(Row, I - Curr_Line'First + 1) := Curr_Line(I);
            end loop;
         end;

         Row := Row + 1;
      end loop;

      Close(Input_File);

      loop
         for I_Row in 1..Line_Count loop
            for I_Col in 1..Line_Length loop
               if CanRemove (Row => I_Row, Col => I_Col) then 
                  LocalCount := LocalCount + 1;
                  Put("x");
                  ProblemSet(I_Row, I_Col) := Empty;
               else 
                  Put(ProblemSet(I_Row, I_Col));
               end if;
            end loop;

            New_Line;
         end loop;

         Total := Total + LocalCount;

         if LocalCount = 0 then
            exit;
         end if;

         LocalCount := 0;
      end loop;

   end;

   New_Line;
   Put_Line ("Total: " & Total'Image);


end Day4_2;