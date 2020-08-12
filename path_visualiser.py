import sys
import time
from laser_tank import LaserTankMap

"""
Path visualiser script.

Use this script to visualise the path of your output files. You should avoid
modifying this file directly.

COMP3702 2020 Assignment 1 Support Code

Last updated by njc 12/08/19
"""


def main(arglist):
    """
    Visualise the path of the given output file applied to the given map file
    :param arglist: map file name, output file name
    """
    if len(arglist) != 2:
        print("Running this file visualises the path of the given output file applied to the given map file.")
        print("Usage: path_visualiser.py [map_file_name] [output_file_name]")
        return

    map_file = arglist[0]
    soln_file = arglist[1]

    game_map = LaserTankMap.process_input_file(map_file)
    game_map.render()

    f = open(soln_file, 'r')
    moves = f.readline().split(',')

    # apply each move in sequence
    for i in range(len(moves)):
        move = moves[i]
        ret = game_map.apply_move(move)
        game_map.render()
        if ret == LaserTankMap.COLLISION:
            print("ERROR: Move resulting in Collision performed at step " + str(i))
        elif ret == LaserTankMap.GAME_OVER:
            print("ERROR: Move resulting in Game Over performed at step " + str(i))
        time.sleep(0.5)

    if game_map.is_finished():
        print("Puzzle solved in " + str(len(moves)) + " steps!")
    else:
        print("ERROR: Goal not reached after all actions performed.")


if __name__ == '__main__':
    main(sys.argv[1:])

