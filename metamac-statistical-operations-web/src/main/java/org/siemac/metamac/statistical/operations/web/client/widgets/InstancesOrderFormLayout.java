package org.siemac.metamac.statistical.operations.web.client.widgets;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;

import java.util.ArrayList;
import java.util.List;

import org.siemac.metamac.statistical.operations.core.dto.InstanceBaseDto;
import org.siemac.metamac.statistical.operations.core.dto.InstanceDto;
import org.siemac.metamac.statistical.operations.web.client.model.ds.InstanceDS;
import org.siemac.metamac.statistical.operations.web.client.utils.RecordUtils;
import org.siemac.metamac.web.common.client.resources.GlobalResources;
import org.siemac.metamac.web.common.client.utils.InternationalStringUtils;
import org.siemac.metamac.web.common.client.widgets.BaseCustomListGrid;
import org.siemac.metamac.web.common.client.widgets.CustomListGridField;
import org.siemac.metamac.web.common.client.widgets.form.GroupDynamicForm;
import org.siemac.metamac.web.common.client.widgets.form.MainFormLayout;
import org.siemac.metamac.web.common.client.widgets.form.fields.ViewTextItem;

import com.smartgwt.client.types.SortDirection;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.form.fields.CanvasItem;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridFieldIfFunction;
import com.smartgwt.client.widgets.grid.ListGridRecord;

public class InstancesOrderFormLayout extends MainFormLayout {

    private BaseCustomListGrid list;
    private ViewTextItem       textItem;

    private String             instancesTitleList;

    public InstancesOrderFormLayout() {
        super();

        // VIEW

        GroupDynamicForm viewForm = new GroupDynamicForm(getConstants().instancesOrder());
        textItem = new ViewTextItem("order", getConstants().instancesOrder());
        viewForm.setFields(textItem);

        instancesTitleList = new String();

        addViewCanvas(viewForm);

        // EDITION

        GroupDynamicForm editionForm = new GroupDynamicForm(getConstants().instancesOrder());
        editionForm.setStyleName("form");

        list = new BaseCustomListGrid();
        list.setCanFocus(false); // To avoid scrolling when a record is clicked
        list.setCanDragSelectText(false);
        list.setCanDragRecordsOut(true);
        list.setCanAcceptDroppedRecords(true);
        list.setCanReorderRecords(true);
        list.setWidth(400);
        list.setCellHeight(24);
        list.setBorder("1px solid #D9D9D9");
        list.setBodyStyleName("normal");
        list.setShowHeader(false);
        CustomListGridField codeField = new CustomListGridField(InstanceDS.CODE, getConstants().instanceCode());
        codeField.setWidth(150);
        CustomListGridField titleField = new CustomListGridField(InstanceDS.TITLE, getConstants().instanceTitle());
        CustomListGridField orderField = new CustomListGridField(InstanceDS.ORDER, getConstants().instanceOrder());
        orderField.setShowIfCondition(new ListGridFieldIfFunction() {

            @Override
            public boolean execute(ListGrid grid, ListGridField field, int fieldNum) {
                return true;
            }
        });
        list.setFields(codeField, titleField, orderField);
        list.setStyleName("orderListStyle");
        list.setSortField(InstanceDS.ORDER);
        list.setSortDirection(SortDirection.DESCENDING);

        CanvasItem canvasItem = new CanvasItem("order", getConstants().instanceOrder());
        canvasItem.setTitleVAlign(VerticalAlignment.TOP);
        canvasItem.setTitleStyle("staticFormItemTitle");
        canvasItem.setCanvas(list);
        canvasItem.setCellHeight(150);

        FormItemIcon infoIcon = new FormItemIcon();
        infoIcon.setSrc(GlobalResources.RESOURCE.info().getURL());
        infoIcon.setPrompt(getConstants().instanceOrderInfoMessage());

        canvasItem.setIcons(infoIcon);
        canvasItem.setIconVAlign(VerticalAlignment.TOP);
        canvasItem.setIconHeight(14);
        canvasItem.setIconWidth(14);

        editionForm.setFields(canvasItem);

        addEditionCanvas(editionForm);
    }

    public void setInstances(List<InstanceBaseDto> instances) {
        StringBuilder builder = new StringBuilder();
        ListGridRecord[] records = new ListGridRecord[instances.size()];
        for (int i = 0; i < instances.size(); i++) {
            builder.append(i != 0 ? ",  " : "");
            builder.append(InternationalStringUtils.getLocalisedString(instances.get(i).getTitle()));
            records[i] = RecordUtils.getInstanceRecord(instances.get(i));
        }
        instancesTitleList = builder.toString();
        textItem.setValue(instancesTitleList);
        list.setData(records);
        list.sort(InstanceDS.ORDER, SortDirection.DESCENDING);
        setViewMode();
    }

    public void addInstance(InstanceDto instanceDto) {
        StringBuilder builder = new StringBuilder();
        builder.append(InternationalStringUtils.getLocalisedString(instanceDto.getTitle()));
        builder.append(instancesTitleList.isEmpty() ? "" : ", ");
        builder.append(instancesTitleList);
        instancesTitleList = builder.toString();
        textItem.setValue(instancesTitleList);
        list.addData(RecordUtils.getInstanceRecord(instanceDto));
        list.sort(InstanceDS.ORDER, SortDirection.DESCENDING);
    }

    public List<Long> getInstancesOrder() {
        List<Long> order = new ArrayList<Long>();
        for (int i = list.getRecords().length - 1; i >= 0; i--) {
            order.add(list.getRecord(i).getAttributeAsLong(InstanceDS.ID));
        }
        list.getRecords();
        return order;
    }
}
