import QtQuick 2.0
import QtQuick.Window 2.0
import QtQuick.Layouts 1.1

Window {
    title: "Visalizer";
    width: 500; height: 500;
    visible: true;

    RowLayout {
        anchors.fill: parent;


        Canvas {
            /* Layout.rightMargin: 5; */
            Layout.fillWidth: true
            Layout.fillHeight: true

            onPaint : {
                var ctx = getContext("2d")

                ctx.save();

                /* Axis */
                ctx.strokeStyle = "black";

                ctx.translate(0, height);
                ctx.scale(width, -height);

                /* TODO: do something with lineWidth */
                var factor = Math.sqrt(width * width + height * height);

                /* 0Y axis */
                ctx.lineWidth = 5 / factor;
                ctx.moveTo(0, 0);
                ctx.lineTo(0, 1);
                ctx.stroke();

                /* 0X axis */
                ctx.lineWidth = 5 / factor;
                ctx.moveTo(0, 0);
                ctx.lineTo(1, 0);
                ctx.stroke();

                ctx.restore();
            }

            Image {
                anchors.fill: parent;
                anchors.margins: 20;
                source: "grid.png";
                opacity: 0.3;
            }


            Canvas {
                id: srcView;
                anchors.fill: parent;
                anchors.margins: 20;
                onPaint : {
                    var model = JSON.parse(modelJSON);

                    var ctx = getContext("2d")
                    ctx.save();

                    ctx.translate(0, height);
                    ctx.scale(width, -height);

                    var factor = Math.sqrt(width * width + height * height);

                    /* Make both src and dst be in the same scale */
                    /* var probWidth = Math.abs(model.dstBBox[0][0] - model.dstBBox[1][0]); */
                    /* var probHeight = Math.abs(model.dstBBox[0][1] - model.dstBBox[1][1]); */
                    /* var figureFactor = Math.max(probWidth, probHeight); */
                    /* ctx.scale(1.0 / figureFactor, 1.0/figureFactor); */

                    var points = model.points;
                    var facets = model.facets;

                    ctx.lineWidth = 5 / factor;
                    ctx.strokeStyle = 'red';
                    ctx.fillStyle = 'rgba(255, 0, 0, 0.5)';

                    var i;
                    for(i = 0; i < facets.length; i++) {
                        var facet = facets[i];
                        ctx.beginPath();

                        var pointIdx = facet[0];
                        var point = points[pointIdx][1];
                        ctx.moveTo(point[0], point[1]);
                        var j;
                        for (j = 1; j < facet.length; j++) {
                            pointIdx = facet[j];
                            point = points[pointIdx][1];
                            ctx.lineTo(point[0], point[1]);
                        }
                        ctx.closePath();
                        ctx.stroke();
                    }

                    ctx.restore();
                }
            }
        }

        Canvas {
            Layout.fillWidth: true
            Layout.fillHeight: true

            onPaint : {
                var ctx = getContext("2d")

                ctx.save();

                /* Axis */
                ctx.strokeStyle = "black";

                ctx.translate(0, height);
                ctx.scale(width, -height);

                /* TODO: do something with lineWidth */
                var factor = Math.sqrt(width * width + height * height);

                /* 0Y axis */
                ctx.lineWidth = 5 / factor;
                ctx.moveTo(0, 0);
                ctx.lineTo(0, 1);
                ctx.stroke();

                /* 0X axis */
                ctx.lineWidth = 5 / factor;
                ctx.moveTo(0, 0);
                ctx.lineTo(1, 0);
                ctx.stroke();

                ctx.restore();
            }

            Image {
                anchors.fill: parent;
                anchors.margins: 20;
                source: "grid.png";
                opacity: 0.3;
            }


            Canvas {
                id: dstView;
                anchors.fill: parent;
                anchors.margins: 20;
                onPaint : {
                    var model = JSON.parse(modelJSON);

                    var ctx = getContext("2d")
                    ctx.save();

                    ctx.translate(0, height);
                    ctx.scale(width, -height);

                    var factor = Math.sqrt(width * width + height * height);

                    /* Start point for problem */
                    var dx = model.dstBBox[0][0];
                    var dy = model.dstBBox[0][1];

                    var probWidth = Math.abs(model.dstBBox[0][0] - model.dstBBox[1][0]);
                    var probHeight = Math.abs(model.dstBBox[0][1] - model.dstBBox[1][1]);
                    var figureFactor = Math.max(probWidth, probHeight);
                    ctx.scale(1.0 / figureFactor, 1.0/figureFactor);

                    var points = model.points;
                    var facets = model.facets;

                    ctx.lineWidth = 5 / factor;
                    ctx.strokeStyle = 'green';
                    ctx.fillStyle = 'rgba(0, 255, 0, 0.3)';

                    var i;
                    for(i = 0; i < facets.length; i++) {
                        var facet = facets[i];
                        ctx.beginPath();

                        var pointIdx = facet[0];
                        var point = points[pointIdx][2];
                        ctx.moveTo(point[0] - dx, point[1] - dy);
                        var j;
                        for (j = 1; j < facet.length; j++) {
                            pointIdx = facet[j];
                            point = points[pointIdx][2];
                            ctx.lineTo(point[0] - dx, point[1] - dy);
                        }
                        ctx.closePath();
                        ctx.stroke();
                        ctx.fill();
                    }

                    ctx.restore();
                }
            }
        }
    }
}
