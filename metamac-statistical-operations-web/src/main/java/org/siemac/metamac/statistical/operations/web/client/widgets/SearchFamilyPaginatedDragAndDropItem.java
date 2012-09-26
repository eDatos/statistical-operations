package org.siemac.metamac.statistical.operations.web.client.widgets;

import java.util.ArrayList;
import java.util.List;

import org.siemac.metamac.statistical.operations.core.dto.FamilyBaseDto;
import org.siemac.metamac.statistical.operations.web.client.model.FamilyRecord;
import org.siemac.metamac.statistical.operations.web.client.model.ds.FamilyDS;
import org.siemac.metamac.statistical.operations.web.client.utils.RecordUtils;
import org.siemac.metamac.web.common.client.utils.FormItemUtils;
import org.siemac.metamac.web.common.client.widgets.actions.PaginatedAction;
import org.siemac.metamac.web.common.client.widgets.form.fields.BaseSearchPaginatedDragAndDropItem;

import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.widgets.grid.HoverCustomizer;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.RecordClickEvent;
import com.smartgwt.client.widgets.grid.events.RecordClickHandler;

public class SearchFamilyPaginatedDragAndDropItem extends BaseSearchPaginatedDragAndDropItem {

    public SearchFamilyPaginatedDragAndDropItem(String name, String title, String dragDropType, int maxResults, PaginatedAction action) {
        super(name, title, maxResults, action);
        create(name, title, maxResults, FormItemUtils.FORM_ITEM_WIDTH, action);
    }

    public SearchFamilyPaginatedDragAndDropItem(String name, String title, int maxResults, int formItemWidth, PaginatedAction action) {
        super(name, title, maxResults, formItemWidth, action);
        create(name, title, maxResults, formItemWidth, action);
    }

    private void create(String name, String title, int maxResults, int formItemWidth, PaginatedAction action) {
        ListGridField codeField = new ListGridField(FamilyDS.CODE);
        codeField.setShowHover(true);
        codeField.setHoverCustomizer(new HoverCustomizer() {

            @Override
            public String hoverHTML(Object value, ListGridRecord record, int rowNum, int colNum) {
                FamilyRecord familyRecord = (FamilyRecord) record;
                return familyRecord != null ? familyRecord.getCode() : new String();
            }
        });

        ListGridField titleField = new ListGridField(FamilyDS.TITLE);
        titleField.setShowHover(true);
        titleField.setHoverCustomizer(new HoverCustomizer() {

            @Override
            public String hoverHTML(Object value, ListGridRecord record, int rowNum, int colNum) {
                FamilyRecord familyRecord = (FamilyRecord) record;
                return familyRecord != null ? familyRecord.getTitle() : new String();
            }
        });

        ListGridField deleteField = new ListGridField(DELETE_FIELD_NAME);
        deleteField.setType(ListGridFieldType.IMAGE);
        deleteField.setWidth("8%");
        deleteField.addRecordClickHandler(new RecordClickHandler() {

            @Override
            public void onRecordClick(RecordClickEvent event) {
                if (isRecordInTargetList((FamilyRecord) event.getRecord())) {
                    targetList.removeData(event.getRecord());
                }
            }
        });

        sourceList.getListGrid().setFields(codeField, titleField);
        targetList.setFields(codeField, deleteField);
    }
    
    @Override
    protected void addNonDuplicatedRecordToTarget(Record record) {
        String code = record.getAttribute(FamilyDS.CODE);
        if (targetList.getRecordList().find(FamilyDS.CODE, code) == null) {
            targetList.addData(record);
        }
    }

    public void setSourceFamilies(List<FamilyBaseDto> familyBaseDtos) {
        FamilyRecord[] records = new FamilyRecord[familyBaseDtos.size()];
        for (int i = 0; i < familyBaseDtos.size(); i++) {
            records[i] = RecordUtils.getFamilyRecord(familyBaseDtos.get(i));
            records[i].setAttribute(DELETE_FIELD_NAME, org.siemac.metamac.web.common.client.resources.GlobalResources.RESOURCE.deleteListGrid().getURL());
        }
        sourceList.getListGrid().setData(records);
    }

    public void setTargetFamilies(List<FamilyBaseDto> familyBaseDtos) {
        clearTargetList();
        for (FamilyBaseDto familyBaseDto : familyBaseDtos) {
            FamilyRecord record = RecordUtils.getFamilyRecord(familyBaseDto);
            record.setAttribute(DELETE_FIELD_NAME, org.siemac.metamac.web.common.client.resources.GlobalResources.RESOURCE.deleteListGrid().getURL());
            targetList.addData(record);
        }
    }

    public List<Long> getSelectedFamilies() {
        List<Long> selectedFamilies = new ArrayList<Long>();
        ListGridRecord[] records = targetList.getRecords();
        for (int i = 0; i < records.length; i++) {
            FamilyRecord record = (FamilyRecord) records[i];
            selectedFamilies.add(record.getId());
        }
        return selectedFamilies;
    }

    private boolean isRecordInTargetList(FamilyRecord record) {
        ListGridRecord[] records = targetList.getRecords();
        for (int i = 0; i < records.length; i++) {
            FamilyRecord familyRecord = (FamilyRecord) records[i];
            if (record.getId() != null && record.getId().equals(familyRecord.getId())) {
                return true;
            }
        }
        return false;
    }

}
