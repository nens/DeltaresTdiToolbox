import copy
import logging

import numpy as np
import pyqtgraph as pg
from PyQt4.QtCore import Qt, QSize, QEvent, QMetaObject
from PyQt4.QtCore import pyqtSignal
from PyQt4.QtGui import QTableView, QWidget, QVBoxLayout, QHBoxLayout, \
    QSizePolicy, QPushButton, QSpacerItem, QApplication, QDockWidget,\
    QComboBox, QColor, QMessageBox
from qgis.core import QgsGeometry, QgsCoordinateTransform
from qgis.core import QgsFeatureRequest

from zDeltaresTdiToolbox.config.waterbalance.sum_configs import serie_settings
from zDeltaresTdiToolbox.models.wb_item import WaterbalanceItemModel
from zDeltaresTdiToolbox.utils.maptools.polygon_draw import PolygonDrawTool

log = logging.getLogger('DeltaresTdi.' + __name__)

try:
    _encoding = QApplication.UnicodeUTF8


    def _translate(context, text, disambig):
        return QApplication.translate(context, text, disambig, _encoding)
except AttributeError:
    def _translate(context, text, disambig):
        return QApplication.translate(context, text, disambig)


serie_settings = {s['name']: s for s in serie_settings}


# some helper functions
#######################

def _get_request_filter(ids):
    ids_flat = list(set([i for j in ids.values() for i in j]))
    return QgsFeatureRequest().setFilterFids(ids_flat)


def _get_feature_iterator(layer, request_filter):
    # mainly pumps are often not present
    if layer:
        return layer.getFeatures(request_filter)
    else:
        return []

#######################


class WaterbalanceItemTable(QTableView):
    hoverExitRow = pyqtSignal(int)
    hoverExitAllRows = pyqtSignal()  # exit the whole widget
    hoverEnterRow = pyqtSignal(str)

    def __init__(self, parent=None):
        super(WaterbalanceItemTable, self).__init__(parent)
        self.setStyleSheet("QTreeView::item:hover{background-color:#FFFF00;}")
        self.setMouseTracking(True)
        self.model = None

        self._last_hovered_row = None
        self.viewport().installEventFilter(self)

    def on_close(self):
        """
        unloading widget and remove all required stuff
        :return:
        """
        self.setMouseTracking(False)
        self.viewport().removeEventFilter(self)

    def closeEvent(self, event):
        """
        overwrite of QDockWidget class to emit signal
        :param event: QEvent
        """
        self.on_close()
        event.accept()

    def eventFilter(self, widget, event):
        if widget is self.viewport():

            if event.type() == QEvent.MouseMove:
                row = self.indexAt(event.pos()).row()
                if row == 0 and self.model and row > self.model.rowCount():
                    row = None

            elif event.type() == QEvent.Leave:
                row = None
                self.hoverExitAllRows.emit()
            else:
                row = self._last_hovered_row

            if row != self._last_hovered_row:
                if self._last_hovered_row is not None:
                    try:
                        self.hover_exit(self._last_hovered_row)
                    except IndexError:
                        log.warning("Hover row index %s out of range",
                            self._last_hovered_row)
                        # self.hoverExitRow.emit(self._last_hovered_row)
                # self.hoverEnterRow.emit(row)
                if row is not None:
                    try:
                        self.hover_enter(row)
                    except IndexError:
                        log.warning("Hover row index %s out of range", row),
                self._last_hovered_row = row
                pass
        return QTableView.eventFilter(self, widget, event)

    def hover_exit(self, row_nr):
        if row_nr >= 0:
            item = self.model.rows[row_nr]
            item.color.value = item.color.value[:3] + [150]
            item.hover.value = False

    def hover_enter(self, row_nr):
        if row_nr >= 0:
            item = self.model.rows[row_nr]
            name = item.name.value
            self.hoverEnterRow.emit(name)
            item.color.value = item.color.value[:3] + [220]
            item.hover.value = True

    def setModel(self, model):
        super(WaterbalanceItemTable, self).setModel(model)

        self.model = model

        self.resizeColumnsToContents()
        self.model.set_column_sizes_on_view(self)


class WaterBalancePlotWidget(pg.PlotWidget):
    def __init__(self, parent=None, name=""):

        super(WaterBalancePlotWidget, self).__init__(parent)
        self.name = name
        self.showGrid(True, True, 0.5)
        self.setLabel("bottom", "tijd", "s")
        self.setLabel("left", "Debiet", "m3/s")
        # Auto SI prefix scaling doesn't work properly with m3, m2 etc.
        self.getAxis("left").enableAutoSIPrefix(False)
        self.series = {}

    def setModel(self, model):
        self.model = model
        self.model.dataChanged.connect(self.data_changed)
        self.model.rowsInserted.connect(self.on_insert)
        self.model.rowsAboutToBeRemoved.connect(
            self.on_remove)

    def on_remove(self):
        self.draw_timeseries()

    def on_insert(self):
        self.draw_timeseries()

    def draw_timeseries(self):
        self.clear()

        ts = self.model.ts

        zeros = np.zeros(shape=(np.size(ts, 0),))
        zero_serie = pg.PlotDataItem(
            x=ts,
            y=zeros,
            connect='finite',
            pen=pg.mkPen(color=QColor(0, 0, 0, 220), width=1))
        self.addItem(zero_serie)

        for dir in ['in', 'out']:
            prev_serie = zeros
            prev_pldi = zero_serie
            for item in self.model.rows:
                if item.active.value:
                    cum_serie = prev_serie + item.ts_series.value[dir]
                    plot_item = pg.PlotDataItem(
                        x=ts,
                        y=cum_serie,
                        connect='finite',
                        pen=pg.mkPen(color=QColor(*item.color.value), width=1))

                    color = item.color.value
                    fill = pg.FillBetweenItem(prev_pldi,
                                              plot_item,
                                              pg.mkBrush(*color))

                    # keep reference
                    item._plots[dir] = plot_item
                    item._plots[dir + 'fill'] = fill
                    prev_serie = cum_serie
                    prev_pldi = plot_item

        for dir in ['in', 'out']:
            for item in reversed(self.model.rows):
                if item.active.value:
                    self.addItem(item._plots[dir])
                    self.addItem(item._plots[dir + 'fill'])

        self.autoRange()

    def data_changed(self, index):
        """
        change graphs based on changes in locations
        :param index: index of changed field
        """
        if self.model.columns[index.column()].name == 'active':
            self.draw_timeseries()

        elif self.model.columns[index.column()].name == 'hover':
            item = self.model.rows[index.row()]
            if item.hover.value:
                if item.active.value:
                    if 'in' in item._plots:
                        item._plots['in'].setPen(color=item.color.value,
                                                 width=1)
                        item._plots['infill'].setBrush(
                            pg.mkBrush(item.color.value))
                    if 'out' in item._plots:
                        item._plots['out'].setPen(color=item.color.value,
                                                  width=1)
                        item._plots['outfill'].setBrush(
                            pg.mkBrush(item.color.value))
            else:
                if item.active.value:
                    if 'in' in item._plots:
                        item._plots['in'].setPen(color=item.color.value,
                                                 width=1)
                        item._plots['infill'].setBrush(
                            pg.mkBrush(item.color.value))
                    if 'out' in item._plots:
                        item._plots['out'].setPen(color=item.color.value,
                                                 width=1)
                    item._plots['outfill'].setBrush(
                        pg.mkBrush(item.color.value))


class WaterBalanceWidget(QDockWidget):
    closingWidget = pyqtSignal()

    INPUT_SERIES = [
        ('2d_in', 0, '2d'),
        ('2d_out', 1, '2d'),
        ('1d_in', 2, '1d'),
        ('1d_out', 3, '1d'),
        ('2d_bound_in', 4, '2d'),
        ('2d_bound_out', 5, '2d'),
        ('1d_bound_in', 6, '1d'),
        ('1d_bound_out', 7, '1d'),
        ('1d_2d_in', 8, '1d'),
        ('1d_2d_out', 9, '2d'),
        ('2d_to_1d_pos', 10, '1d_2d'),
        ('2d_to_1d_neg', 11, '1d_2d'),
        ('pump_in', 12, '1d'),
        ('pump_out', 13, '1d'),
        ('rain', 14, '2d'),
        ('infiltration_rate_simple', 15, '2d'),
        ('lat_2d', 16, '2d'),
        ('lat_1d', 17, '1d'),
        ('d_2d_vol', 18, '2d'),
        ('d_1d_vol', 19, '1d'),
        ('error_2d', 20, 'error_2d'),
        ('error_1d', 21, 'error_1d'),
        ('error_1d_2d', 22, 'error_1d_2d'),
        ('2d_groundwater_in', 23, '2d'),
        ('2d_groundwater_out', 24, '2d'),
        ('d_2d_groundwater_vol', 25, '2d'),
        ('leak', 26, '2d'),
    ]

    def __init__(self, parent=None, iface=None, ts_datasource=None, wb_calc=None):
        """Constructor."""
        super(WaterBalanceWidget, self).__init__(parent)

        self.aggregation_warning_issued_on_start = False
        self.iface = iface
        self.ts_datasource = ts_datasource
        self.calc = wb_calc

        # setup ui
        self.setup_ui(self)

        self.model = WaterbalanceItemModel()
        self.wb_item_table.setModel(self.model)
        self.plot_widget.setModel(self.model)

        # link tool
        self.polygon_tool = PolygonDrawTool(self.iface.mapCanvas(),
                                            self.select_polygon_button,
                                            self.on_polygon_ready)

        # fill comboboxes with selections
        self.modelpart_combo_box.insertItems(
            0,
            ['1d 2d', '1d', '2d'])
        #  self.modelpart_combo_box.setCurrentIndex(0)
        self.source_nc_combo_box.insertItems(
            0,
            ['normal', 'aggregation'])
        self.sum_type_combo_box.insertItems(
            0,
            serie_settings.keys())

        self.agg_combo_box.insertItems(
            0,
            ['m3/s', 'm3 aggregated'])

        # add listeners
        self.select_polygon_button.toggled.connect(self.toggle_polygon_button)
        self.reset_waterbalans_button.clicked.connect(self.reset_waterbalans)
        # self.polygon_tool.deactivated.connect(self.update_wb)
        self.modelpart_combo_box.currentIndexChanged.connect(self.update_wb)
        self.source_nc_combo_box.currentIndexChanged.connect(self.update_wb)
        self.source_nc_combo_box.currentIndexChanged.connect(
            self.issue_warning)
        self.sum_type_combo_box.currentIndexChanged.connect(self.update_wb)
        self.agg_combo_box.currentIndexChanged.connect(self.update_wb)
        self.wb_item_table.hoverEnterRow.connect(
            self.hover_enter_map_visualization)
        self.wb_item_table.hoverExitAllRows.connect(
            self.hover_exit_map_visualization)

        # TODO: is this a good default?
        # initially turn on tool
        self.select_polygon_button.toggle()

    def hover_enter_map_visualization(self, name):
        types_2d_line = [
            '2d flow',
        ]
        types_2d_node = [
            'volume verandering 2d',
        ]
        # TODO 1: generate this dict

        # TODO 2: name alone is not unique enough, e.g. there are two '2d flow'
        # entries.
        # SO THIS IS INCORRECT AND NEEDS TO BE FIXED ASAP!

        # TODO 3: using the name as key is INCREDIBLY error prone: one
        # spelling mistake or a change in sum_configs and it doesn't work
        # anymore, and because we also catch the KeyErrors you won't even
        # notice. NEEDS TO BE FIXED
        name_to_line_types = {
            '2d flow': ['2d'],
            '2d boundaries': ['2d_bound'],
            '1d flow': ['1d'],
            '1d boundaries': ['1d_bound'],
            '1d-2d uitwisseling': ['1d_2d'],
            '1d-2d flow door grens': ['1d_2d'],
            'pompen': ['pump_or_whatever'],  # TODO: fix this magic string
            '2d groundwater flow': ['2d_groundwater'],
        }
        name_to_node_types = {
            'volume verandering': ['1d', '2d', '2d_groundwater'],
            'volume verandering 2d': ['2d'],
            'volume verandering 1d': ['1d'],
            'volume verandering 2d grondwater': ['2d_groundwater'],
            'neerslag': ['2d'],
            'lateraal 1d': ['1d'],
            'lateraal 2d': ['2d'],
            'leakage': ['2d'],
            'infiltratie': ['2d'],
            'belasting (regen en lateralen)': ['1d', '2d'],
        }
        try:
            types_line = name_to_line_types[name]
            line_geoms = []
            for tl in types_line:
                geoms = self.qgs_lines[tl]
                line_geoms.extend(geoms)
        except KeyError:
            line_geoms = []
        try:
            types_node = name_to_node_types[name]
            point_geoms = []
            for t in types_node:
                geoms = self.qgs_points[t]
                point_geoms.extend(geoms)
        except KeyError:
            point_geoms = []
        self.polygon_tool.selection_vis.update(line_geoms, point_geoms)

    def hover_exit_map_visualization(self, *args):
        self.polygon_tool.selection_vis.reset()

    def on_polygon_ready(self, points):
        self.iface.mapCanvas().unsetMapTool(self.polygon_tool)

    def reset_waterbalans(self):
        self.polygon_tool.reset()

    def toggle_polygon_button(self):

        if self.select_polygon_button.isChecked():
            self.reset_waterbalans()

            self.iface.mapCanvas().setMapTool(self.polygon_tool)

            self.select_polygon_button.setText(_translate(
                "DockWidget", "Gebied tekenen afronden", None))
        else:
            self.iface.mapCanvas().unsetMapTool(self.polygon_tool)
            self.update_wb()
            self.select_polygon_button.setText(_translate(
                "DockWidget", "Teken nieuw gebied", None))

            if not self.aggregation_warning_issued_on_start:
                self.issue_warning()
                self.aggregation_warning_issued_on_start = True

    def redraw_wb(self):
        pass

    def update_wb(self):

        try:
            ts, graph_series = self.calc_wb(
                self.modelpart_combo_box.currentText(),
                self.source_nc_combo_box.currentText(),
                self.agg_combo_box.currentText(),
                serie_settings[self.sum_type_combo_box.currentText()])
        except self.calc.AggregationFileNotFoundError:
            QMessageBox.warning(
                None,
                "No aggregation file found",
                "The 'aggregation' option requires an aggregation NetCDF "
                "file with the following variables:"
                "\n\ncumulative:\n- rain\n- infiltration\n- laterals"
                "\n- leakage\n- discharge\n- pump discharge"
                "\n\npositive cumulative:\n- discharge"
                "\n\nnegative cumulative:\n- discharge"
            )
            return

        self.model.removeRows(0, len(self.model.rows))

        self.model.ts = ts
        self.model.insertRows(graph_series['items'])

        if self.agg_combo_box.currentText() == 'm3/s':
            self.plot_widget.setLabel("left", "Debiet", "m3/s")
        elif self.agg_combo_box.currentText() == 'm3 aggregated':
            self.plot_widget.setLabel("left", "Cumulatieve debiet", "m3")
        else:
            self.plot_widget.setLabel("left", "-", "-")

        # set labels for in and out fluxes
        text_upper = pg.TextItem(text="in", anchor=(0, 1), angle=-90)
        text_upper.setPos(0, 0)
        text_lower = pg.TextItem(text="uit", anchor=(1, 1), angle=-90)
        text_lower.setPos(0, 0)
        self.plot_widget.addItem(text_upper)
        self.plot_widget.addItem(text_lower)

    def calc_wb(self, model_part, source_nc, aggregation_type, settings):
        poly_points = self.polygon_tool.points
        wb_polygon = QgsGeometry.fromPolygon([poly_points])

        lines, points, pumps = \
            self.ts_datasource.rows[0].get_result_layers()
        tr = QgsCoordinateTransform(
            self.iface.mapCanvas().mapRenderer().destinationCrs(), lines.crs())
        wb_polygon.transform(tr)

        link_ids, pump_ids = self.calc.get_incoming_and_outcoming_link_ids(
            wb_polygon, model_part)
        node_ids = self.calc.get_nodes(wb_polygon, model_part)

        ts, total_time = self.calc.get_aggregated_flows(
            link_ids, pump_ids, node_ids, model_part, source_nc)

        graph_series = self.make_graph_series(
            ts, total_time, model_part, aggregation_type, settings)

        self.prepare_and_visualize_selection(
            link_ids, pump_ids, node_ids, lines, pumps, points)

        return ts, graph_series

    def prepare_and_visualize_selection(
            self, link_ids, pump_ids, node_ids, lines, pumps, points,
            draw_it=False):
        req_filter_links = _get_request_filter(link_ids)
        req_filter_pumps = _get_request_filter(pump_ids)
        req_filter_nodes = _get_request_filter(node_ids)

        line_id_to_type = {}
        for _type, id_list in link_ids.items():
            for i in id_list:
                # we're not interested in in or out types
                t = _type.rsplit('_out')[0].rsplit('_in')[0]
                line_id_to_type[i] = t
        # pump_id_to_type = {}
        # for _, id_list in pump_ids.items():
        #     for i in id_list:
        #         pump_id_to_type[i] = 'all'
        node_id_to_type = {}
        for _type, id_list in node_ids.items():
            for i in id_list:
                node_id_to_type[i] = _type

        qgs_lines = {}
        qgs_points = {}
        tr_reverse = QgsCoordinateTransform(
            lines.crs(),
            self.iface.mapCanvas().mapRenderer().destinationCrs(),
        )

        # NOTE: getting all features again isn't efficient because they're
        # already calculated in WaterBalanceCalculation, but w/e
        for feat in _get_feature_iterator(lines, req_filter_links):
            geom = feat.geometry()
            geom.transform(tr_reverse)
            _type = line_id_to_type[feat['id']]
            try:
                qgs_lines[_type]
            except KeyError:
                qgs_lines[_type] = []
            qgs_lines[_type].append(geom.asPolyline())
        qgs_lines['pump_or_whatever'] = []
        for feat in _get_feature_iterator(pumps, req_filter_pumps):
            geom = feat.geometry()
            geom.transform(tr_reverse)
            qgs_lines['pump_or_whatever'].append(geom.asPolyline())
        for feat in _get_feature_iterator(points, req_filter_nodes):
            geom = feat.geometry()
            geom.transform(tr_reverse)
            _type = node_id_to_type[feat['id']]
            try:
                qgs_points[_type]
            except KeyError:
                qgs_points[_type] = []
            qgs_points[_type].append(geom.asPoint())

        self.qgs_lines = qgs_lines
        self.qgs_points = qgs_points

        # draw the lines/points immediately
        # TODO: probably need to throw this code away since we won't use it
        if draw_it:
            qgs_lines_all = [j for i in qgs_lines.values() for j in i]
            qgs_points_all = [j for i in qgs_points.values() for j in i]

            self.polygon_tool.update_line_point_selection(
                qgs_lines_all, qgs_points_all)

    def make_graph_series(
            self, ts, total_time, model_part, aggregation_type, settings):
        settings = copy.deepcopy(settings)

        if model_part == '1d 2d':
            input_series = dict([
                (x, y) for (x, y, z) in self.INPUT_SERIES
                if z in ['2d', '1d', 'error_1d_2d']])
        elif model_part == '2d':
            input_series = dict([
                (x, y) for (x, y, z) in self.INPUT_SERIES
                if z in ['2d', '1d_2d', 'error_2d']])
        elif model_part == '1d':
            input_series = dict([
                (x, y) for (x, y, z) in self.INPUT_SERIES
                if z in ['1d', '1d_2d', 'error_1d']])
            idx_2d_to_1d_pos = input_series['2d_to_1d_pos']
            idx_2d_to_1d_neg = input_series['2d_to_1d_neg']
            idx_2d_to_1d = (idx_2d_to_1d_pos, idx_2d_to_1d_neg)
            total_time[:, idx_2d_to_1d] = total_time[:, idx_2d_to_1d] * -1

        for serie_setting in settings.get('items', []):
            serie_setting['active'] = True
            serie_setting['method'] = serie_setting['default_method']
            serie_setting['color'] = [int(c) for c in serie_setting['def_color'].split(',')] + [150]
            serie_setting['ts_series'] = {}
            nrs_input_series = []
            for serie in serie_setting['series']:
                if serie not in input_series:
                    # throw good error message
                    log.warning('serie config error: %s is an unknown serie or is doubled in the config.', serie)
                else:
                    nrs_input_series.append(input_series[serie])
                    del input_series[serie]

            if serie_setting['default_method'] == 'net':
                sum = total_time[:, nrs_input_series].sum(axis=1)
                serie_setting['ts_series']['in'] = sum.clip(min=0)
                serie_setting['ts_series']['out'] = sum.clip(max=0)
            elif serie_setting['default_method'] == 'gross':
                sum_pos = np.zeros(shape=(np.size(ts, 0),))
                sum_neg = np.zeros(shape=(np.size(ts, 0),))
                for nr in nrs_input_series:
                    sum_pos += total_time[:, nr].clip(min=0)
                    sum_neg += total_time[:, nr].clip(max=0)
                serie_setting['ts_series']['in'] = sum_pos
                serie_setting['ts_series']['out'] = sum_neg
            else:
                # throw config error
                log.warning('aggregation %s method unknown.', serie_setting['default_method'])

            if aggregation_type == 'm3 aggregated':
                log.debug('aggregate')
                diff = np.append([0], np.diff(ts))

                serie_setting['ts_series']['in'] = serie_setting['ts_series']['in'] * diff
                serie_setting['ts_series']['in'] = np.cumsum(serie_setting['ts_series']['in'], axis=0)

                serie_setting['ts_series']['out'] = serie_setting['ts_series']['out'] * diff
                serie_setting['ts_series']['out'] = np.cumsum(serie_setting['ts_series']['out'], axis=0)

        if len(input_series) > 0:

            serie_setting = {
                'name': 'Overige',
                'default_method': settings['remnant_method'],
                'order': 100,
                'color': [int(c) for c in settings['remnant_def_color'].split(',')] + [150],
                'def_color': settings['remnant_def_color'], #TODO: fix + [150],
                'series': [key for key in input_series],
                'ts_series': {}
            }
            for serie in input_series:
                nrs_input_series.append(input_series[serie])

            if serie_setting['default_method'] == 'net':
                sum = total_time[:, nrs_input_series].sum(axis=1)
                serie_setting['ts_series']['in'] = sum.clip(min=0)
                serie_setting['ts_series']['out'] = sum.clip(max=0)
            elif serie_setting['default_method'] == 'gross':
                sum_pos = np.zeros(shape=(np.size(ts, 0),))
                sum_neg = np.zeros(shape=(np.size(ts, 0),))
                for nr in nrs_input_series:
                    sum_pos += total_time[:, nr].clip(min=0)
                    sum_neg += total_time[:, nr].clip(max=0)
                serie_setting['ts_series']['in'] = sum_pos
                serie_setting['ts_series']['out'] = sum_neg
            else:
                # throw config error
                log.warning('aggregation %s method unknown.', serie_setting['default_method'])

            settings['items'].append(serie_setting)

        if model_part == '1d':
            total_time[:, (10, 11)] = total_time[:, (10, 11)] * -1

        settings['items'] = sorted(settings['items'], key=lambda item: item['order'])

        return settings

    def unset_tool(self):
        pass

    def accept(self):
        pass

    def reject(self):
        self.close()

    def closeEvent(self, event):
        self.select_polygon_button.toggled.disconnect(
            self.toggle_polygon_button)
        self.reset_waterbalans_button.clicked.disconnect(
            self.reset_waterbalans)
        # self.polygon_tool.deactivated.disconnect(self.update_wb)
        self.iface.mapCanvas().unsetMapTool(self.polygon_tool)
        self.polygon_tool.close()

        self.modelpart_combo_box.currentIndexChanged.disconnect(self.update_wb)
        self.source_nc_combo_box.currentIndexChanged.disconnect(self.update_wb)
        self.sum_type_combo_box.currentIndexChanged.disconnect(self.update_wb)
        self.wb_item_table.hoverEnterRow.disconnect(
            self.hover_enter_map_visualization)
        self.wb_item_table.hoverExitAllRows.disconnect(
            self.hover_exit_map_visualization)

        self.closingWidget.emit()
        event.accept()

    def issue_warning(self):
        mode = self.source_nc_combo_box.currentText()
        if mode == 'normal':
            QMessageBox.information(
                None,
                "Information",
                "You're currently using the 'normal' NetCDF result file which "
                "may result in a less accurate overall water balance "
                "compared to the 'aggregation' NetCDF file, depending on the "
                "output time step that was chosen. Proceed at your own "
                "discretion. Please select the 'aggregation' option "
                "for the most accurate results."
            )

    def setup_ui(self, dock_widget):
        """
        initiate main Qt building blocks of interface
        :param dock_widget: QDockWidget instance
        """

        dock_widget.setObjectName("dock_widget")
        dock_widget.setAttribute(Qt.WA_DeleteOnClose)

        self.dock_widget_content = QWidget(self)
        self.dock_widget_content.setObjectName("dockWidgetContent")

        self.main_vlayout = QVBoxLayout(self)
        self.dock_widget_content.setLayout(self.main_vlayout)

        # add button to add objects to graphs
        self.button_bar_hlayout = QHBoxLayout(self)
        self.select_polygon_button = QPushButton(self)
        self.select_polygon_button.setCheckable(True)
        self.select_polygon_button.setObjectName("SelectedSideview")
        self.button_bar_hlayout.addWidget(self.select_polygon_button)

        self.reset_waterbalans_button = QPushButton(self)
        self.reset_waterbalans_button.setObjectName("ResetSideview")
        self.button_bar_hlayout.addWidget(self.reset_waterbalans_button)
        self.modelpart_combo_box = QComboBox(self)
        self.button_bar_hlayout.addWidget(self.modelpart_combo_box)
        self.source_nc_combo_box = QComboBox(self)
        self.button_bar_hlayout.addWidget(self.source_nc_combo_box)
        self.sum_type_combo_box = QComboBox(self)
        self.button_bar_hlayout.addWidget(self.sum_type_combo_box)

        self.agg_combo_box = QComboBox(self)
        self.button_bar_hlayout.addWidget(self.agg_combo_box)

        spacer_item = QSpacerItem(40,
                                  20,
                                  QSizePolicy.Expanding,
                                  QSizePolicy.Minimum)
        self.button_bar_hlayout.addItem(spacer_item)
        self.main_vlayout.addLayout(self.button_bar_hlayout)

        # add tabWidget for graphWidgets
        self.contentLayout = QHBoxLayout(self)

        # Graph
        self.plot_widget = WaterBalancePlotWidget(self)
        sizePolicy = QSizePolicy(QSizePolicy.Expanding, QSizePolicy.Expanding)
        sizePolicy.setHorizontalStretch(1)
        sizePolicy.setVerticalStretch(1)
        sizePolicy.setHeightForWidth(
            self.plot_widget.sizePolicy().hasHeightForWidth())
        self.plot_widget.setSizePolicy(sizePolicy)
        self.plot_widget.setMinimumSize(QSize(250, 250))

        self.contentLayout.addWidget(self.plot_widget)

        # table
        self.wb_item_table = WaterbalanceItemTable(self)
        sizePolicy = QSizePolicy(QSizePolicy.Fixed, QSizePolicy.Expanding)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(
            self.wb_item_table.sizePolicy().hasHeightForWidth())
        self.wb_item_table.setSizePolicy(sizePolicy)
        self.wb_item_table.setMinimumSize(QSize(250, 0))

        self.contentLayout.addWidget(self.wb_item_table)

        self.main_vlayout.addLayout(self.contentLayout)

        # add dockwidget
        dock_widget.setWidget(self.dock_widget_content)
        self.retranslate_ui(dock_widget)
        QMetaObject.connectSlotsByName(dock_widget)

    def retranslate_ui(self, dock_widget):
        pass
        dock_widget.setWindowTitle(_translate(
            "DockWidget", "3Di waterbalans", None))
        self.select_polygon_button.setText(_translate(
            "DockWidget", "Teken nieuw gebied", None))
        self.reset_waterbalans_button.setText(_translate(
            "DockWidget", "Verberg op kaart", None))
