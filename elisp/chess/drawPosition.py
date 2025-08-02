import os, sys, re, argparse
from chess import Board, InvalidMoveError, IllegalMoveError, AmbiguousMoveError
from PIL import Image, ImageDraw

board_png = "./board.png"
pieces_png = {
    "P" : "img/white-pawn.png",
    "N" : "img/white-knight.png",
    "B" : "img/white-bishop.png",
    "R" : "img/white-rook.png",
    "Q" : "img/white-queen.png",
    "K" : "img/white-king.png",
    "p" : "img/black-pawn.png",
    "n" : "img/black-knight.png",
    "b" : "img/black-bishop.png",
    "r" : "img/black-rook.png",
    "q" : "img/black-queen.png",
    "k" : "img/black-king.png"
}

script_dir = os.path.dirname(os.path.realpath(__file__))

for key, path in pieces_png.items():
    pieces_png[key] = os.path.join(script_dir, path)

parser = argparse.ArgumentParser()
parser.add_argument('-w', '--white', default="#ffffff", type=str, help='color for white box')
parser.add_argument('-b', '--black', default="#1f1f1f", type=str, help='color for black box')
parser.add_argument('-p', '--pixelsize', default=1160, type=int, help='size for png image on px')
parser.add_argument('-o', '--output', default="./board.png", type=str, help="path of png image")
parser.add_argument('-s', '--swap', action='store_true', help='swap orientation of board')
parser.add_argument('-m', '--movements', type=str, help='movements for apply to board')

def isEven(n):
    return n % 2 == 0

def isOdd(n):
    return not isEven(n)

def isMayus(char):
    return char == char.upper()

def isWhiteBox(row, col):
    return (isEven(row) and isEven(col)) or (isOdd(row) and isOdd(col))

def reverseStr(s):
    return s[::-1]

def drawBoard(fen, swap_orientation, size, wcolor, bcolor, output):
    img = Image.new("RGBA", (size, size))
    row = 0
    col = 0
    x = 0
    y = 0
    boxsize = size//8

    if swap_orientation:
        pfen = prossecedFen(reverseStr(fen))
    else:
        pfen = prossecedFen(fen)

    for c in pfen:
        x = col * boxsize
        y = row * boxsize
        color = wcolor if isWhiteBox(row, col) else bcolor

        img1 = ImageDraw.Draw(img)
        img1.rectangle([x,y,x + boxsize, y + boxsize], fill=color)

        if c in pieces_png.keys():
            piece = Image.open(pieces_png.get(c), "r").convert("RGBA").copy().resize((boxsize, boxsize))
            img.paste(piece, (x,y), piece)

        if (col == 7):
            col = 0
            row += 1
        else:
            col += 1

    img.save(output, format="png")

def prossecedFen(fen):
    pfen = ""
    fun = lambda c : "0" * int(c) if c.isnumeric() else c
    
    for c in fen.replace("/", ""):
        pfen = pfen + fun(c)
        
    return pfen

def replaceMayusPieces(move):
    es_pieces = "CATDR"
    en_pieces = "NBRQK"

    if move[0] in es_pieces:
        return move.replace(move[0], en_pieces[es_pieces.index(move[0])]) 
    else:
        return move

def main():

    args = parser.parse_args()    
    board = Board()
    movements = args.movements
    movements = re.findall("[A-Za-z0-8+\\-]{2,5}", movements)

    for move in movements:
        if isMayus(move[0]):
           move = replaceMayusPieces(move)

        try:
           board.push_san(move)
        except IllegalMoveError as e:
            print(f"Error: {e} is a illegal move.")
            exit(1)
        except InvalidMoveError as e:
            print(f"Error: {e} is syntactically invalid move.")
            exit(1)
        except AmbiguousMoveError as e:
            print(f"Error {e} is a ambiguous move.")
            exit(1)
        
    drawBoard(board.board_fen(), args.swap, args.pixelsize, args.white, args.black, args.output)
    exit(0)

if __name__ == '__main__':
    main()
