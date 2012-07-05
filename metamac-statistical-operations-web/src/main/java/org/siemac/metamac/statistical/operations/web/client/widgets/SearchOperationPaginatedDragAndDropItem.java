package org.siemac.metamac.statistical.operations.web.client.widgets;

import java.util.ArrayList;
import java.util.List;

import org.siemac.metamac.statistical.operations.core.dto.OperationBaseDto;
import org.siemac.metamac.statistical.operations.web.client.model.OperationRecord;
import org.siemac.metamac.statistical.operations.web.client.model.ds.OperationDS;
import org.siemac.metamac.statistical.operations.web.client.utils.RecordUtils;
import org.siemac.metamac.web.common.client.utils.FormItemUtils;
import org.siemac.metamac.web.common.client.widgets.actions.PaginatedAction;
import org.siemac.metamac.web.common.client.widgets.form.fields.BaseSearchPaginatedDragAndDropItem;

import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.widgets.grid.HoverCustomizer;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.RecordClickEvent;
import com.smartgwt.client.widgets.grid.events.RecordClickHandler;

public class SearchOperationPaginatedDragAndDropItem extends BaseSearchPaginatedDragAndDropItem {

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
                return operationRecord != null ? operationRecord.getCode() : new String();
            }
        });

        ListGridField titleField = new ListGridField(OperationDS.OP_TITLE);
        titleField.setShowHover(true);
        titleField.setHoverCustomizer(new HoverCustomizer() {

            @Override
            public String hoverHTML(Object value, ListGridRecord record, int rowNum, int colNum) {
                OperationRecord operationRecord = (OperationRecord) record;
                return operationRecord != null ? operationRecord.getTitle() : new String();
            }
        });

        ListGridField deleteField = new ListGridField(DELETE_FIELD_NAME);
        deleteField.setType(ListGridFieldType.IMAGE);
        deleteField.setWidth("8%");
        deleteField.addRecordClickHandler(new RecordClickHandler() {

            @Override
            public void onRecordClick(RecordClickEvent event) {
                if (isRecordInTargetList((OperationRecord) event.getRecord())) {
                    targetList.removeData(event.getRecord());
                }
            }
        });

        sourceList.getListGrid().setFields(codeField, titleField);
        targetList.setFields(codeField, deleteField);
    }

    public void setSourceOperations(List<OperationBaseDto> operationBaseDtos) {
        OperationRecord[] records = new OperationRecord[operationBaseDtos.size()];
        for (int i = 0; i < operationBaseDtos.size(); i++) {
            records[i] = RecordUtils.getOperationRecord(operationBaseDtos.get(i));
            records[i].setAttribute(DELETE_FIELD_NAME, org.siemac.metamac.web.common.client.resources.GlobalResources.RESOURCE.deleteListGrid().getURL());
        }
        sourceList.getListGrid().setData(records);
    }

    public void setTargetOperations(List<OperationBaseDto> operationBaseDtos) {
        clearTargetList();
        for (OperationBaseDto operationBaseDto : operationBaseDtos) {
            OperationRecord record = RecordUtils.getOperationRecord(operationBaseDto);
            record.setAttribute(DELETE_FIELD_NAME, org.siemac.metamac.web.common.client.resources.GlobalResources.RESOURCE.deleteListGrid().getURL());
            targetList.addData(record);
        }
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

    private boolean isRecordInTargetList(OperationRecord record) {
        ListGridRecord[] records = targetList.getRecords();
        for (int i = 0; i < records.length; i++) {
            OperationRecord operationRecord = (OperationRecord) records[i];
            if (record.getId() != null && record.getId().equals(operationRecord.getId())) {
                return true;
            }
        }
        return false;
    }

}
