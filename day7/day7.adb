with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Containers.Vectors;

procedure Day7 is
   Input_File : File_Type;
   File_Name : String := "input.txt";

   Splits : Integer := 0;
   TimeLines : Long_Integer := 0;
begin
   Open (File => Input_File, Mode => In_File, Name => File_Name);

   declare 
      Curr_Line : String := Get_Line(Input_File);
      type RayArray is array (Curr_Line'Range) of Long_Integer;
      Rays : RayArray;
   begin
      for I in Rays'Range loop
         Rays(I) := 0;
      end loop;

      while not End_Of_File(Input_File) loop
         for I in Curr_Line'Range loop
            case Curr_Line(I) is
               when 'S' => 
                  Rays(I) := Rays(I) + Long_Integer(1);
               when '^' => 
                  if Rays(I) > 0 then
                     Rays(I + 1) := Rays(I + 1) + Rays(I);
                     Rays(I - 1) := Rays(I - 1) + Rays(I);

                     Rays(I) := 0;

                     Splits := Splits + 1;
                  end if;
               when others => null;
            end case;
         end loop;

         for I in Rays'Range loop
            if Rays(I) > 0 then 
               Put("|");
            else
               Put(Curr_Line(I) & "");
            end if;
         end loop;

         Curr_Line := Get_Line(Input_File);

         New_Line;
      end loop;

      for I in Rays'Range loop 
         TimeLines := TimeLines + Long_Integer(Rays(I));
      end loop;
   end;

   Put_Line ("Total splits: " & Splits'Image);
   Put_Line ("Total timelines: " & TimeLines'Image);
end Day7;