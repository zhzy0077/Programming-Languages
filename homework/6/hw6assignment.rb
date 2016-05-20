# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_Pieces = [
      # [[[0, 0], [1, 0], [0, 1], [1, 1]]], # square (only needs one)
      # rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
      # [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
      #  [[0, 0], [0, -1], [0, 1], [0, 2]]],
      # rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
      # rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
      # rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
      # rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
      rotations([[0, 0], [1, 0], [2, 0], [0, 1], [1, 1], [2, 1]]), # fat L
      [[[0, 0], [-1, 0], [1, 0], [2, 0], [3, 0]], # longer (only needs two)
       [[0, 0], [0, -1], [0, 1], [0, 2], [0, 3]]],
      rotations([[0, 0], [0, 1], [1, 0]]) # small L
  ]
  CHEAT_PIECE = [[[0, 0]]]
  @@cheat = false
  # your enhancements here
  def self.next_piece (board)
    if @@cheat
      MyPiece.new(CHEAT_PIECE, board)
      @@cheat = false
    else
      piece = All_Pieces.sample
      puts "Choose #{piece}"
      MyPiece.new(piece, board)
    end
  end

  def self.cheat= val
    @@cheat = val
  end
end

class MyBoard < Board
  # your enhancements here
  def initialize(game)
    super(game)
    @current_block = MyPiece.next_piece(self)
    @game = game
    @score = 0
  end

  def cheat
    if @score < 100
      false
    else
      @score -= 100
      MyPiece.cheat = true
    end
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..4).each { |index|
      if locations[index] == nil
        return
      end
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] =
          @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end


  def rotate_sym
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  def next_piece
    @current_block = MyPiece.next_piece(self)
    @current_pos = nil
  end


end

class MyTetris < Tetris
  # your enhancements here
  def initialize
    super
    @root = TetrisRoot.new
  end

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super
    @root.bind('u', lambda { @board.rotate_sym })
    @root.bind('c', lambda { @board.cheat })
  end
end


