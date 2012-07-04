package org.siemac.metamac.statistical.operations.web.client.widgets;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.siemac.metamac.statistical.operations.core.dto.OperationBaseDto;
import org.siemac.metamac.statistical.operations.web.client.model.OperationRecord;
import org.siemac.metamac.statistical.operations.web.client.model.ds.OperationDS;
import org.siemac.metamac.statistical.operations.web.client.utils.RecordUtils;
import org.siemac.metamac.web.common.client.utils.FormItemUtils;
import org.siemac.metamac.web.common.client.widgets.actions.PaginatedAction;
import org.siemac.metamac.web.common.client.widgets.form.fields.BaseSearchPaginatedDragAndDropItem;

import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.grid.HoverCustomizer;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.RecordClickEvent;
import com.smartgwt.client.widgets.grid.events.RecordClickHandler;
import com.smartgwt.client.widgets.grid.events.RecordDoubleClickEvent;
import com.smartgwt.client.widgets.grid.events.RecordDoubleClickHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

public class SearchOperationPaginatedDragAndDropItem extends BaseSearchPaginatedDragAndDropItem {

    private Set<OperationRecord> targetRecords = new HashSet<OperationRecord>();

    public SearchOperationPaginatedDragAndDropItem(String name, String title, String dragDropType, int maxResults, PaginatedAction action) {
        super(name, title, dragDropType, maxResults, action);
        create(name, title, dragDropType, maxResults, FormItemUtils.FORM_ITEM_WIDTH, action);
    }

    public SearchOperationPaginatedDragAndDropItem(String name, String title, String dragDropType, int maxResults, int formItemWidth, PaginatedAction action) {
        super(name, title, dragDropType, maxResults, formItemWidth, action);
        create(name, title, dragDropType, maxResults, formItemWidth, action);
    }

    private void create(String name, String title, String dragDropType, int maxResults, int formItemWidth, PaginatedAction action) {

        ListGridField codeField = new ListGridField(OperationDS.OP_CODE);
        codeField.setShowHover(true);
        codeField.setHoverCustomizer(new HoverCustomizer() {

            @Override
            public String hoverHTML(Object value, ListGridRecord record, int rowNum, int colNum) {
                OperationRecord operationRecord = (OperationRecord) record;
                return operationRecord.getCode();
            }
        });

        ListGridField titleField = new ListGridField(OperationDS.OP_TITLE);
        titleField.setShowHover(true);
        titleField.setHoverCustomizer(new HoverCustomizer() {

            @Override
            public String hoverHTML(Object value, ListGridRecord record, int rowNum, int colNum) {
                OperationRecord operationRecord = (OperationRecord) record;
                return operationRecord.getTitle();
            }
        });

        ListGridField deleteField = new ListGridField("delete");
        deleteField.setType(ListGridFieldType.IMAGE);
        deleteField.setWidth("8%");
        deleteField.addRecordClickHandler(new RecordClickHandler() {

            @Override
            public void onRecordClick(RecordClickEvent event) {
                if (targetRecords.contains(event.getRecord())) {
                    targetRecords.remove(event.getRecord());
                }
                updateTargetRecordList();
            }
        });

        sourceList.getListGrid().setFields(codeField, titleField);
        sourceList.getListGrid().setDataSource(new OperationDS());
        sourceList.getListGrid().addRecordDoubleClickHandler(new RecordDoubleClickHandler() {

            @Override
            public void onRecordDoubleClick(RecordDoubleClickEvent event) {
                targetRecords.add(((OperationRecord) event.getRecord()));
                updateTargetRecordList();
            }
        });

        targetList.setFields(codeField, deleteField);
        targetList.setDataSource(new OperationDS());

        rightImg.addClickHandler(new ClickHandler() {

            public void onClick(ClickEvent event) {
                ListGridRecord[] records = sourceList.getListGrid().getSelectedRecords();
                for (ListGridRecord record : records) {
                    targetRecords.add(((OperationRecord) record));
                }
                updateTargetRecordList();
            }
        });

        rightAll.addClickHandler(new ClickHandler() {

            public void onClick(ClickEvent event) {
                ListGridRecord[] records = sourceList.getListGrid().getRecords();
                for (ListGridRecord record : records) {
                    targetRecords.add(((OperationRecord) record));
                }
                updateTargetRecordList();
            }
        });

        HLayout hLayout = new HLayout(1);
        hLayout.addMember(sourceList);
        hLayout.addMember(buttonStack);
        hLayout.addMember(targetList);

        VLayout vLayout = new VLayout();
        vLayout.addMember(form);
        vLayout.addMember(hLayout);

        setCanvas(vLayout);
    }

    @Override
    public void clearValue() {
        targetRecords = new HashSet<OperationRecord>();
        updateTargetRecordList();
    }

    public void setSourceOperations(List<OperationBaseDto> operationBaseDtos) {
        OperationRecord[] records = new OperationRecord[operationBaseDtos.size()];
        for (int i = 0; i < operationBaseDtos.size(); i++) {
            records[i] = RecordUtils.getOperationRecord(operationBaseDtos.get(i));
            records[i].setAttribute("delete", org.siemac.metamac.web.common.client.resources.GlobalResources.RESOURCE.deleteListGrid().getURL());
        }
        sourceList.getListGrid().setData(records);
    }

    public void setTargetOperations(List<OperationBaseDto> operationBaseDtos) {
        OperationRecord[] records = new OperationRecord[operationBaseDtos.size()];
        for (int i = 0; i < operationBaseDtos.size(); i++) {
            records[i] = RecordUtils.getOperationRecord(operationBaseDtos.get(i));
        }
        targetList.setData(records);
    }

    public List<Long> getSelectedOperations() {
        List<Long> selectedOperations = new ArrayList<Long>();
        ListGridRecord[] records = targetList.getRecords();
        for (int i = 0; i < records.length; i++) {
            OperationRecord record = (OperationRecord) records[i];
            selectedOperations.add(record.getId());
        }
        return selectedOperations;
    }

    private void updateTargetRecordList() {
        targetList.setData(targetRecords.toArray(new OperationRecord[0]));
    }

}
