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
                ctx.lineWidth = 2 / factor;

                var i = 0;
                for (i = 0; i < polygons.length; i++) {
                    switch (polygons[i].type) {
                    case 'fill':
                        ctx.strokeStyle = 'red';
                        ctx.fillStyle = 'rgba(255, 0, 0, 0.5)';
                        break;
                    case 'hole':
                        ctx.strokeStyle = 'yellow';
                        ctx.fillStyle = 'rgba(0, 255, 255, 0.7)';
                        break;
                    }

                    var points = polygons[i].points;
                    if (points.length > 0) {
                        ctx.beginPath();

                        ctx.moveTo(points[0][0], points[0][1]);

                        var j = 1;
                        for (j = 1; j < points.length; j++) {
                            ctx.lineTo(points[j][0], points[j][1]);
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
        }
    }

}
