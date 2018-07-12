from PyQt4.QtCore import Qt
from PyQt4.QtGui import QColor, QCursor

from qgis.core import QGis, QgsGeometry
from qgis.gui import QgsRubberBand, QgsVertexMarker, QgsMapTool

RGBA = 255, 0, 0


class SelectionVisualisation(object):
    """Visualize selected lines and points. """
    def __init__(self, canvas):
        self.canvas = canvas

        self.rb_line = QgsRubberBand(self.canvas, QGis.Line)
        self.rb_line.setColor(Qt.red)
        self.rb_line.setColor(QColor(*RGBA))
        self.rb_line.setLineStyle(Qt.DotLine)
        self.rb_line.setWidth(3)

        self.vertex_markers = []

        self.lines = []
        self.points = []

        self.reset()

    def show(self):
        # visualize lines
        multiline = QgsGeometry().fromMultiPolyline(self.lines)
        self.rb_line.setToGeometry(multiline, None)
        # visualize points
        for p in self.points:
            marker = QgsVertexMarker(self.canvas)
            marker.setCenter(p)
            marker.setIconType(QgsVertexMarker.ICON_BOX)
            marker.setColor(QColor(*RGBA))
            marker.setVisible(True)
            self.vertex_markers.append(marker)

    def reset(self):
        self.rb_line.reset(QGis.Line)
        for m in self.vertex_markers:
            m.setVisible(False)
        self.vertex_markers = []
        self.lines = []
        self.points = []

    def update(self, lines, points):
        """lines and points are lists of QgsPoints and QgsPolylines."""
        self.reset()
        self.lines = lines
        self.points = points
        self.show()


class PolygonDrawMapVisualisation(object):

    def __init__(self, canvas):

        self.canvas = canvas
        self.points = []

        # temp layer for side profile trac
        self.rb = QgsRubberBand(self.canvas, QGis.Polygon)
        self.rb.setColor(Qt.red)
        self.rb.setFillColor(QColor(255, 0, 0, 64))
        self.rb.setLineStyle(Qt.SolidLine)
        self.rb.setWidth(1)
        self.reset()

    def close(self):
        self.points = []
        self.reset()

    def show(self):
        self.rb.show()

    def hide(self):
        self.rb.hide()

    def add_point(self, point):
        self.points.append(point)
        self.rb.addPoint(point, True)
        self.rb.show()

    def reset(self):
        self.points = []
        self.rb.reset(QGis.Polygon)


class PolygonDrawTool(QgsMapTool):
    def __init__(self, canvas, button, callback_on_draw_finish):
        QgsMapTool.__init__(self, canvas)
        self.canvas = canvas
        self.callback_on_draw_finish = callback_on_draw_finish

        self.isEmittingPoint = False

        self.map_visualisation = PolygonDrawMapVisualisation(self.canvas)
        self.selection_vis = SelectionVisualisation(self.canvas)
        self.setButton(button)

    def activate(self):
        super(PolygonDrawTool, self).activate()
        self.canvas.setCursor(QCursor(Qt.CrossCursor))

    def reset(self):
        self.startPoint = self.endPoint = None
        self.isEmittingPoint = False
        self.map_visualisation.reset()
        self.selection_vis.reset()

    def canvasDoubleClickEvent(self, e):
        self.callback_on_draw_finish(self.map_visualisation.points)

    def canvasPressEvent(self, e):
        point = self.toMapCoordinates(e.pos())
        self.isEmittingPoint = True

    def canvasReleaseEvent(self, e):
        point = self.toMapCoordinates(e.pos())
        self.map_visualisation.add_point(point)
        self.isEmittingPoint = False

    def canvasMoveEvent(self, e):
        if not self.isEmittingPoint:
            return

    def deactivate(self):
        super(PolygonDrawTool, self).deactivate()
        self.canvas.setCursor(QCursor(Qt.ArrowCursor))

    def close(self):
        self.deactivate()
        self.map_visualisation.close()

    @property
    def points(self):
        return self.map_visualisation.points

    def update_line_point_selection(self, lines, points):
        """Update the SelectionVisualisation (i.e., the selected lines and
        points).
        """
        self.selection_vis.update(lines, points)


    # def activate(self):
    #     self.canvas.setCursor(QCursor(Qt.CrossCursor))
    #
    # def deactivate(self):
    #     self.deactivated.emit()
    #     self.canvas.setCursor(QCursor(Qt.ArrowCursor))
    #
    # def isZoomTool(self):
    #     return False
    #
    # def isTransient(self):
    #     return False
    #
    # def isEditTool(self):
    #     return False
