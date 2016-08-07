#include "mainwindow.h"
#include <iostream>

int main(int argc, char *argv[])
{
	if(argc != 3 && argc != 4)
	{
		std::cout << "USAGE: ./visualizer problem.txt image.png\n       ./visualizer problem.txt solution.txt image.png" <<std::endl;
		exit(1);
	}

	Drawer w;
	if(argc == 3)
		w.DrawProblem(argv[1], 0, argv[2]);
	if(argc == 4)
		w.DrawProblem(argv[1], argv[2], argv[3]);
//	w.DrawProblem("/Users/fedor/PROJECTS/Fun/icfpc-2016/problems/90.txt", "1.png");
	exit(0);
}
