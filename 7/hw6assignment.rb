# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyTetris < Tetris
  # your enhancements here
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super
    @root.bind('u', proc {@board.rotate_180})
    @root.bind('c', proc {@board.cheat})
  end
end

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = All_Pieces + 
                  [rotations([[0, 0], [1, 0], [2, 0], [0, 1], [1, 1]]), # stubby finger shape
                  [[[0, 0], [-1, 0], [-2, 0], [1, 0], [2, 0]], # extra long thing
                  [[0, 0], [0, -1], [0, -2], [0, 1], [0, 2]]],
                  rotations([[0, 0], [1, 0], [0, 1]])] # little l shape

  Cheat_Piece = [[[0, 0]]]
  # your enhancements here
  def self.next_piece (board, cheat_enabled=false)
    if cheat_enabled
      MyPiece.new(Cheat_Piece, board)
    else
      MyPiece.new(All_My_Pieces.sample, board)
    end
  end

end

class MyBoard < Board
  # your enhancements here
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
    @cheat = false
  end

  def cheat
    if @score >= 100 and !@cheat
      @cheat = true
      @score -= 100
      @game.update_score
    end
  end

  def next_piece
    @current_block = MyPiece.next_piece(self, @cheat)
    @cheat = false
    @current_pos = nil
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(locations.size - 1)).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def rotate_180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end
end