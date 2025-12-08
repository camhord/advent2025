with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure Day4 is
   File_Name : constant String := "input.txt";
   Roll : constant Character := '@';
   Empty : constant Character := '.';

   Input_File : File_Type;

   type Buffer_Index is mod 3;
   Buf_Index : Buffer_Index := 0;

   type LocalBuffer is array (Buffer_Index) of Unbounded_String;
   LocalField : LocalBuffer;

   Curr_Line : Unbounded_String;

   Local_Count : Integer := 0;
   Thresh : constant Integer := 4;

   Total : Integer := 0;

   TopLine : Boolean := true;
   BottomLine : Boolean := false;

   procedure CheckIndex (Line : Unbounded_String; Index : Integer; Count : in out Integer) is
   begin 
       if Element(Line, Integer(Index)) = Roll then
         Count := Count + 1;
      end if;
   end CheckIndex;
begin
   Open(File => Input_File, Mode => In_File, Name => File_Name);

   LocalField(LocalField'First) := To_Unbounded_String(Get_Line(Input_File));

   loop
      if not End_Of_File(Input_File) then
         LocalField(LocalField'First + Buf_Index + 1) := To_Unbounded_String(Get_Line(Input_File));
      end if;

      Curr_Line := LocalField(LocalField'First + Buf_Index);
      for I in To_String(Curr_Line)'Range loop
         if Element(Curr_Line, I) = Roll then
            -- Check behind
            if I > To_String(Curr_Line)'First then
               CheckIndex (Curr_Line, I - 1, Local_Count);
            end if;

            -- Check ahead
            if I < To_String(Curr_Line)'Last then
               CheckIndex (Curr_Line, I + 1, Local_Count);
            end if;

            -- check above line
            if not TopLine then
               CheckIndex (LocalField(LocalField'First + Buf_Index - 1), I, Local_Count);

               if I > To_String(Curr_Line)'First then
                  CheckIndex (LocalField(LocalField'First + Buf_Index - 1), I - 1, Local_Count);
               end if;

               if I < To_String(Curr_Line)'Last then
                  CheckIndex (LocalField(LocalField'First + Buf_Index - 1), I + 1, Local_Count);
               end if;
            end if;

            -- check below line
            if not BottomLine then
               CheckIndex (LocalField(LocalField'First + Buf_Index + 1), I, Local_Count);

               if I > To_String(Curr_Line)'First then
                  CheckIndex (LocalField(LocalField'First + Buf_Index + 1), I - 1, Local_Count);
               end if;

               if I < To_String(Curr_Line)'Last then
                  CheckIndex (LocalField(LocalField'First + Buf_Index + 1), I + 1, Local_Count);
               end if;
            end if;

            if Local_Count < Thresh then
               Total := Total + 1;
               Put("x");
            else 
               Put(Trim(Source => Local_Count'Image, Side => Ada.Strings.Both));
            end if;

         else 
            Put(".");
         end if;

         -- reset local count for next check
         Local_Count := 0;
         
      end loop;

      TopLine := false;

      if BottomLine then
         exit;
      end if;

      if End_Of_File(Input_File) then
         BottomLine := true;
      end if;
      
      Buf_Index := Buf_Index + 1;

      Put_Line ("");

   end loop;
   
   Put_Line ("");
   Put_Line ("");

   Put_Line ("Total: " & Total'Image);


end Day4;