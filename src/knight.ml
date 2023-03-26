
let node1 lst =
  let nth = List.nth lst in
  
  (abs((nth 0)-(nth 2)) = 1 ) && (abs((nth 1)- (nth 3))= 2) ||
  (abs((nth 0)-(nth 2)) = 2 ) && (abs((nth 1)- (nth 3))= 1)  


let validate_knight_move (brd:Board.board) (plr : Player.player) (lst:int list) =
  node1 lst
