with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Strings.Maps;  use Ada.Strings.Maps;


procedure Day10 is 
   type Bits is mod 2**16;

   Input_File : File_Type;
   File_Name : String := "input.txt";

   Whitespace : constant Character_Set := To_Set (' ');
   Comma : constant Character_Set := TO_Set(',');

   package ButtonsVector is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Bits);
   Buttons : ButtonsVector.Vector;  

   function To_String(B : Bits) return String is
      SOut : String  (1..16);
   begin
      for I in 0..15 loop
         if (2**I and B) > 0 then 
            SOut(16 - I) := '#';
         else 
            SOut(16 - I) := '.';
         end if;
      end loop;
      
      return B'Image & SOut;
   end To_String;

   -- AI-written subset getter - SHAME!
   function Subsets(Size : Positive) return ButtonsVector.Vector is
      -- Helper function to handle the recursion with an index tracker
      function Recurse(K : Positive; Start_Index : Integer) return ButtonsVector.Vector is
         Result, Temp : ButtonsVector.Vector;
      begin
         -- Optimization: If we need more elements than are remaining, return empty
         if (Buttons.Last_Index - Start_Index + 1) < K then
            return Result;
         end if;

         -- Base Case: If we need size 1, return all individual elements remaining
         if K = 1 then
            for I in Start_Index .. Buttons.Last_Index loop
               Result.Append(Buttons(I));
            end loop;
            return Result;
         end if;

         -- Recursive Step
         for I in Start_Index .. Buttons.Last_Index loop
            -- Get subsets of size K-1 from the REST of the vector (I + 1)
            Temp := Recurse(K - 1, I + 1);

            -- XOR the current button 'I' with the results from the recursion
            for J in Temp.First_Index .. Temp.Last_Index loop
               Result.Append(New_Item => Temp(J) xor Buttons(I));
            end loop;
         end loop;

         return Result;
      end Recurse;

   begin
      -- Kick off recursion starting at the first index of the global Buttons vector
      return Recurse(Size, Buttons.First_Index);
   end Subsets;

   EndState : Bits;

   Total : Natural := 0;
begin
   Open(Input_File, In_File, File_Name);

   while not End_Of_File(Input_File) loop
      Buttons.Clear;
      declare
         Line : String := Get_Line(Input_File);
         F   : Positive;
         L   : Natural;
         I   : Natural := 1;
      begin
         while I in Line'Range loop
            Find_Token
               (Source  => Line,
                  Set     => Whitespace,
                  From    => I,
                  Test    => Outside,
                  First   => F,
                  Last    => L);

            exit when L = 0;

            case Line(F) is 
               when '[' => -- parse end state
                  EndState := 0;

                  for E in reverse F+1..L-1 loop
                     EndState := EndState * 2;
                     if Line(E) = '#' then
                        EndState := EndState + 1;
                     end if;
                  end loop;
                  
                  Put ("[" & To_String(EndState) & "] ");
               when '(' => -- parse button
                  declare
                     Button : Bits := 0;
                     Buffer : Unbounded_String;
                  begin
                     for B in F+1..L loop
                        case Line(B) is
                           when ',' | ')' =>
                              Button := Button + (2**Integer'Value(To_String(Buffer)));
                              Buffer := Null_Unbounded_String;
                           when others =>
                              Buffer := Buffer & String'(1 =>Line(B));
                        end case;
                     end loop;

                     Buttons.Append (New_Item => Button);
                     Put ("(" & To_String(Button) & ") ");
                  end;
               when others =>
                  null;
               end case;

            I := L + 1;
         end loop;

         New_Line;

         -- Calcuate buttons to press
         declare 
            Current : Bits := 0;
            Presses : Positive := 1;
            Test : ButtonsVector.Vector;
         begin 
            while Current /= EndState loop
               Test := Subsets(Presses);

               for B in Test.First_Index..Test.Last_Index loop
                  if (Test(B) xor EndState) = 0 then 
                     Put_Line (To_String(EndState) & " = " & To_String (Test(B)));
                     Current := EndState;
                     Total := Total + Presses;
                     exit;
                  end if;
               end loop;

               Put_Line (Presses'Image);
               Presses := Presses + 1;
            end loop;
         end;
      end;
   end loop;
   Put_Line ("Total:" & Total'Image);

end Day10;