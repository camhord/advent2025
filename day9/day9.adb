with Ada.Containers;
with Ada.Containers.Hashed_Sets;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Containers.Vectors;

procedure Day9 is
   type Point is record
      X, Y : Integer;
   end record;

   function Point_Equ(R, L : Point) return Boolean is
   begin
      return R.X = L.X and R.Y = L.Y;
   end Point_Equ;
   
   function PointHash(P : Point) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type( P.X * 31 + P.Y );
   end PointHash;

   package PointVector is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Point);
   Points : PointVector.Vector;

   package PointHashSet is new Ada.Containers.Hashed_Sets
     (Element_Type        => Point,
      Hash                => PointHash,
      Equivalent_Elements => Point_Equ);
   GreenPoints : PointHashSet.Set;

   package IntegerVector is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Long_Integer);
   package IntVecSort is new IntegerVector.Generic_Sorting;
   Areas : IntegerVector.Vector;

   function To_String(N : Point) return String is 
   begin
      return "(" & N.X'Image & "," & N.Y'Image & ")"; 
   end To_String;

   function ParseNode (Point_String : String) return Point is 
      To_Return : Point;
      First : Positive := Point_String'First;
      Last : Natural;
   begin
      Last := Ada.Strings.Fixed.Index(Point_String(First..Point_String'Last), ",");
      To_Return.X := Integer'Value(Point_String(First..Last - 1));

      To_Return.Y := Integer'Value(Point_String(Last + 1..Point_String'Last));

      return To_Return;
   end ParseNode;

   function CheckCol(X, MinY, MaxY : Integer; MinP : Point) return Boolean is
      In_Loop, Latch : Boolean := false;
   begin
      for Y in MinP.Y..MaxY loop
         if Y > MinY and not In_Loop and not Latch then 
            --Put_Line(To_String((X,Y)) & "IN: " & In_Loop'Image & "FAIL OUT");
            return false;
         end if;

         if GreenPoints.Contains ((X,Y)) then
            if Latch then
               In_Loop := not In_Loop;
               Latch := false;
            else 
               Latch := not GreenPoints.Contains ((X,Y - 1));
            end if;
         else
            if Latch then 
               In_Loop := not In_Loop;
               Latch := false;
            end if;
         end if;
         --Put_Line(To_String((X,Y)) & "IN: " & In_Loop'Image);
      end loop;

      return In_Loop;

   end CheckCol;

   function CheckRow(Y, MinX, MaxX : Integer; MinP : Point) return Boolean is
      In_Loop, Latch : Boolean := false;
   begin
      for X in MinP.X..MaxX loop
         if X > MinX and not In_Loop and not Latch then
            --Put_Line(To_String((X,Y)) & "IN: " & In_Loop'Image & "FAIL OUT");
            return false;
         end if;

         if GreenPoints.Contains ((X,Y)) then
            if Latch then
               In_Loop := not In_Loop;
               Latch := false;
            else 
               Latch := not GreenPoints.Contains ((X - 1,Y));
            end if;
         else
            if Latch then 
               In_Loop := not In_Loop;
               Latch := false;
            end if;
         end if;
         --Put_Line(To_String((X,Y)) & "IN: " & In_Loop'Image);
      end loop;

      return In_Loop;

   end CheckRow;

   type Side is (Top, Right, Left, Bottom);

   function Contains(BigL, BigR, LilL, LilR : Point) return Boolean is
      BigMin, BigMax, LilMin, LilMax : Integer := 0;
   begin
      if BigL.X = BigR.X and BigL.X = LilL.X and BigL.X = LilR.X then
         BigMin := Integer'Min(BigL.Y, BigR.Y);
         BigMax := Integer'Max(BigL.Y, BigR.Y);
         LilMin := Integer'Min(LilL.Y, LilR.Y);
         LilMax := Integer'Max(LilL.Y, LilR.Y);
      elsif BigL.Y = BigR.Y and BigL.Y = LilL.Y and BigL.Y = LilR.Y then
         BigMin := Integer'Min(BigL.X, BigR.X);
         BigMax := Integer'Max(BigL.X, BigR.X);
         LilMin := Integer'Min(LilL.X, LilR.X);
         LilMax := Integer'Max(LilL.X, LilR.X);
      end if;

      return BigMin <= LilMin and BigMax >= LilMax and not (BigMin = LilMin and BigMax = LilMax);
   end Contains;

   function SinglValueContains(RL, RR, P : Point) return Boolean is
   begin
      if RL.X = RR.X then 
         return Integer'Min(RL.Y, RR.Y) < P.Y and Integer'Max(RL.Y, RR.Y) > P.Y;
      else
         return Integer'Min(RL.X, RR.X) < P.X and Integer'Max(RL.X, RR.X) > P.X;
      end if;
   end SinglValueContains;

   function CheckIntersection(L, R : Point; S : Side) return Boolean is
      Curr, Last, Next, DLast : Point;
      Horizontal : Boolean := L.Y = R.Y;
   begin
      for I in Points.First_Index..Points.Last_Index loop
         if I = Points.First_Index then
            Last := Points(Points.Last_Index);
         else 
            Last := Points(I - 1);
         end if;
         Curr := Points(I);
         if I = Points.Last_Index then 
            Next := Points(Points.First_Index);
         else 
            Next := Points(I + 1);
         end if;
         if I < Points.First_Index + 2 then
            DLast := Points(Points.Last_Index - (I - Points.First_Index));
         else 
            DLast := Points(I - 2);
         end if;

         if Horizontal and Curr.X = Last.X then
            if Integer'Min(Last.Y, Curr.Y) <= L.Y and Integer'Max(Last.Y, Curr.Y) >= L.Y then
               if Integer'Min(L.X, R.X) <= Last.X and Integer'Max(L.X, R.X) >= Last.X then
                  if S = Bottom and ((Last.Y = L.Y and Curr.Y > L.Y) or (Curr.Y = L.Y and Last.Y > L.Y)) then
                     if L.X /= Last.X and R.X /= Last.X then
                        --Put_Line ("Fail 1");
                        return true;
                     end if;
                  elsif S = Top and ((Last.Y = L.Y and Curr.Y < L.Y) or (Curr.Y = L.Y and Last.Y < L.Y)) then 
                     if L.X /= Last.X and R.X /= Last.X then
                        --Put_Line ("Fail 2");
                        return true;
                     end if;
                  elsif L.Y /= Curr.Y and L.Y /= Last.Y and Curr.X /= L.X and Curr.X /= R.X then
                        --Put_Line ("Fail 3");
                        --Put_Line (To_String(L) & "-" & To_String (R) & " and " & To_String (Last) & "-" & To_String (Curr));
                     return true;
                  end if;
               end if;
            end if;
         elsif not Horizontal and Curr.Y = Last.Y then
            if Integer'Min(Last.X, Curr.X) < L.X and Integer'Max(Last.X, Curr.X) > L.X then
               if Integer'Min(L.Y, R.Y) < Last.Y and Integer'Max(L.Y, R.Y) > Last.Y then
                  if S = Left and ((Last.X = L.X and Curr.X > L.X) or (Curr.X = L.X and Last.X > L.X)) then
                     if L.Y /= Last.Y and R.Y /= Last.Y then 
                        --Put_Line ("Fail 4");
                        return true;
                     end if;
                  elsif S = Right and ((Last.X = L.X and Curr.X < L.X) or (Curr.X = L.X and Last.X < L.X)) then 
                     if L.Y /= Last.Y and R.Y /= Last.Y then 
                        --Put_Line ("Fail 5");
                        return true;
                     end if;
                  elsif L.X /= Curr.X and L.X /= Last.X and Curr.Y /= L.Y and Curr.Y /= R.Y then
                        --Put_Line ("Fail 6");
                     return true;
                  end if;
               end if;
            end if;
         elsif Contains (BigL => L, BigR => R, LilL => Last, LilR => Curr) then 
            --Put_Line (To_String(L) & "-" & To_String (R) & " Contains " & To_String (Last) & "-" & To_String (Curr));
            case S is
               when Top =>
                  if (SinglValueContains(L, R, Curr) and Next.Y < L.Y) or (SinglValueContains(L, R, Last) and DLast.Y < L.Y) then 
                     --Put_Line("Fail 7");
                     return true;
                  end if;
               when Bottom => 
                  if (SinglValueContains(L, R, Curr) and Next.Y > L.Y) or (SinglValueContains(L, R, Last) and DLast.Y > L.Y) then 
                     --Put_Line("Fail 8");
                     return true;
                  end if;
               when Left => 
                  if (SinglValueContains(L, R, Curr) and Next.X > L.X) or (SinglValueContains(L, R, Last) and DLast.X > L.X) then 
                     --Put_Line("Fail 9");
                     return true;
                  end if;
               when Right =>
                  if (SinglValueContains(L, R, Curr) and Next.X < L.X) or (SinglValueContains(L, R, Last) and DLast.X < L.X) then 
                     --Put_Line("Fail 10");
                     return true;
                  end if;
               when others =>
                  return true;
            end case;
         end if;
      end loop;

      return false;
   end CheckIntersection;

   function CheckBounds(L, R, MinP: Point) return Boolean is 
      MinX, MinY, MaxX, MaxY : Integer;
   begin
      if L.X = R.X or L.Y = R.Y then 
         --Put_Line ("Fail Line");
         return false;
      end if;

      MinX := Integer'Min(L.X, R.X);
      MaxX := Integer'Max(L.X, R.X);
      MinY := Integer'Min(L.Y, R.Y);
      MaxY := Integer'Max(L.Y, R.Y);

      return not CheckIntersection ((MinX, MaxY), (MinX, MinY), Left) and
            not CheckIntersection ((MinX, MaxY), (MaxX, MaxY), Top) and
            not CheckIntersection ((MinX, MinY), (MaxX, MinY), Bottom) and
            not CheckIntersection ((MaxX, MaxY), (MaxX, MinY), Right);

      --return CheckRow (L.Y, Integer'Min(L.X, R.X), Integer'Max(L.X, R.X), MinP) and 
      --         CheckRow (R.Y, Integer'Min(L.X, R.X), Integer'Max(L.X, R.X), MinP) and 
      --         CheckCol (L.X, Integer'Min(L.Y, R.Y), Integer'Max(L.Y, R.Y), MinP) and 
      --         CheckCol (R.X, Integer'Min(L.Y, R.Y), Integer'Max(L.Y, R.Y), MinP);
   end CheckBounds;

   function Area(L, R, MinP: Point) return Long_Integer is 
   begin
      if not CheckBounds (L, R, MinP) then 
         return 0; --return Long_Integer(0);
      end if;

      return Long_Integer(Abs(L.X - R.X) + 1) * Long_Integer(Abs(L.Y - R.Y) + 1);
   end Area;

   procedure AddPoints(Current, Last : Point) is 
      NewGreen : Point;
   begin 
      Points.Append(New_Item => Current);

      --if Current.Y = Last.Y then 
      --   for I in Integer'Min(Current.X, Last.X)..Integer'Max(Current.X, Last.X) loop
      --      NewGreen := (I, Current.Y);
      --      if not GreenPoints.Contains(NewGreen) then 
      --         GreenPoints.Insert(New_Item => NewGreen);
      --      end if;
      --   end loop;
      --else 
      --   for I in Integer'Min(Current.Y, Last.Y)..Integer'Max(Current.Y, Last.Y) loop
      --      NewGreen := (Current.X, I);
      --      if not GreenPoints.Contains(NewGreen) then 
      --         GreenPoints.Insert(New_Item => NewGreen);
      --      end if;
      --   end loop;
      --end if;
   end AddPoints;

   Input_File : File_Type;
   File_Name : String := "input.txt";

   Min, Max : Point;
begin
   Open(Input_File, In_File, File_Name);

   declare
      Current, Last, First : Point;
   begin
      First := ParseNode(Get_Line(Input_File));

      Min := First;
      Max := First;

      Last := First;

      while not End_Of_File(Input_File) loop
         Current := ParseNode(Get_Line (Input_File));
         if Current.X <= Min.X then
            Min.X := Current.X - 1;
         elsif Current.X >= Max.X then
            Max.X := Current.X + 1;
         end if;
         if Current.Y <= Min.Y then
            Min.Y := Current.Y - 1;
         elsif Current.Y >= Max.Y then
            Max.Y := Current.Y + 1;
         end if;

         AddPoints (Current, Last);
         Last := Current;
      end loop;

      AddPoints (First, Current);
   end;

   --for Y in Min.Y..Max.Y loop
   --   for X in Min.X..Max.X loop
   --      if GreenPoints.Contains ((X, Y)) then 
   --         Put ("#");
   --      else
   --         Put (".");
   --      end if;
   --   end loop;
   --   New_Line;
   --end loop;

   -- brute force loop fill
   --declare
   --   In_Loop, Latch : Boolean := False;
   --begin
   --   for Y in Min.Y..Max.Y loop
   --      for X in Min.X..Max.X loop
   --         if not In_Loop then 
   --            if GreenPoints.Contains ((X,Y)) then
   --               Latch := true;
   --            else
   --               if Latch then 
   --                  In_Loop := true;
   --                  Latch := false;
   --                  if not GreenPoints.Contains ((X,Y)) then 
   --                     GreenPoints.Insert (New_Item => (X,Y));
   --                  end if;
   --               end if;
   --            end if;
   --         else
   --            if not GreenPoints.Contains ((X,Y)) then 
   --               GreenPoints.Insert (New_Item => (X,Y));
   --            else 
   --               In_Loop := false;
   --            end if;
   --         end if;
   --      end loop;
   --      In_Loop := false;
   --      Latch := false;
   --   end loop;
   --end;

   declare 
      Area_I : Long_Integer;
   begin
      for I in Points.First_Index..Points.Last_Index loop
         for J in I + 1..Points.Last_Index loop
            Area_I := Area(Points(I), Points(J), Min);
            if Area_I > 0 then
               Put_Line (To_String(Points(I)) & To_String(Points(J)) & Long_Integer'Image(Area_I));
            end if;
            Areas.Append(New_Item => Area_I);
         end loop;
      end loop;
   end;

   --Areas.Append(New_Item => Area((2,8), (7,6), Min));

   IntVecSort.Sort (Areas);

   Put_Line (Long_Integer'Image(Areas(Areas.Last_Index)));

   --for Y in Min.Y..Max.Y loop
   --   for X in Min.X..Max.X loop
   --      if GreenPoints.Contains ((X, Y)) then 
   --         Put ("#");
   --      else
   --         Put (".");
   --      end if;
   --   end loop;
   --   New_Line;
   --end loop;
end Day9;