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

                /* Problem polygons */
                var i = 0; var polygons = problem.polygons;
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
                /* Skeleton lines */
                ctx.strokeStyle = 'rgba(100, 100, 100, 0.8)';
                for (i = 0; i < problem.skeleton.length; i++) {
                    var line = problem.skeleton[i];
                    ctx.moveTo(line[0][0], line[0][1]);
                    ctx.lineTo(line[1][0], line[1][1]);
                    ctx.closePath();
                    ctx.stroke();
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

            ctx.restore();
        }

        Image {
            id: field;
            anchors.fill: parent;
            anchors.margins: 20;
            source: "grid.png";
            opacity: 0.3;
        }


        Loader {
            anchors.fill: parent;
            anchors.margins: 20;

            sourceComponent: figure;
            enabled: true;

            property var problem: JSON.parse(problemJSON);
        }
    }

}
