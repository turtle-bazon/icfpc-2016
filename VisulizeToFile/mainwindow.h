#ifndef DRAWER_H
#define DRAWER_H

#include <QVector>
#include <QPointF>
#include <QLineF>

class Drawer
{
public:
	typedef QVector<QPointF> Polygon;	
	typedef QVector<Polygon> Polygons;
	typedef QVector<QLineF> Skeleton;
	typedef QVector<QPointF> Points;
	typedef QVector<int> Facet;
	typedef QVector<Facet> Facets;

	struct Problem
	{
		Polygons polygons;
		Skeleton skeleton;
	};

	struct Solution
	{
		Points src;
		Points dst;
		Facets facets;
	};

public:
	Problem ReadProblem(const char * Src);
	Solution ReadSolution(const char * Src);

	void DrawProblem(const char * ProblemSrc, const char * SolutionSrc, const char * Dst);
};

#endif // DRAWER_H
