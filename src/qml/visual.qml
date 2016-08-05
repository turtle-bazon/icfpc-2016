import QtQuick 2.0
import QtQuick.Window 2.0

Window {
    title: "Visalizer";
    width: 500; height: 500;
    visible: true;

    Component {
        id: figure;

        Canvas {
            onPaint : {
                var ctx = getContext("2d")
                ctx.save();

                ctx.translate(0, parent.height);
                ctx.scale(parent.width, -parent.height);

                var factor = Math.sqrt(parent.width * parent.width + parent.height * parent.height);
                ctx.lineWidth = lineWidth / factor;
                ctx.strokeStyle = strokeStyle;
                ctx.fillStyle = fillStyle;

                var i = 0;
                for (i = 0; i < polygons.length; i++) {
                    var points = polygons[i];
                    if (points.length > 0) {
                        ctx.beginPath();

                        ctx.moveTo(points[0][0], points[0][1]);

                        var i = 1;
                        for (i = 1; i < points.length; i++) {
                            ctx.lineTo(points[i][0], points[i][1]);
                        }

                        ctx.closePath();

                        ctx.fill();
                        ctx.stroke();
                    }
                }

                ctx.restore();
            }
        }
    }

    Canvas {
        id: axis;
        anchors.fill: parent;
        anchors.margins: 10;

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

            /* TODO: Grid */

            ctx.restore();
        }

        Image {
            id: field;
            anchors.fill: parent;
            anchors.margins: 20;
            source: "grid.png"
        }

        Loader {
            anchors.fill: parent;
            anchors.margins: 20;

            sourceComponent: figure;
            enabled: true;

            /* property var points: [[0,0], [1,0], [0.5, 0.5], [0, 0.5]]; */
            property var polygons: JSON.parse(pointsJSON);
            property int lineWidth: 2;
            property string strokeStyle: 'red';
            property string fillStyle: 'rgba(255, 0, 0, 0.5)';
        }
    }

}
