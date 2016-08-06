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

	struct Problem
	{
		Polygons polygons;
		Skeleton skeleton;
	};

public:
	Problem ReadProblem(const char * Src);

	void DrawProblem(const char * Src, const char * Dst);
};

#endif // DRAWER_H
