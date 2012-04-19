package org.siemac.metamac.statistical.operations.web.client.operation.view;

import java.util.ArrayList;
import java.util.List;

import org.siemac.metamac.core.common.dto.ExternalItemBtDto;
import org.siemac.metamac.domain.statistical.operations.dto.OperationBaseDto;
import org.siemac.metamac.domain.statistical.operations.dto.OperationDto;
import org.siemac.metamac.statistical.operations.web.client.OperationsWeb;
import org.siemac.metamac.statistical.operations.web.client.model.OperationRecord;
import org.siemac.metamac.statistical.operations.web.client.operation.presenter.OperationListPresenter;
import org.siemac.metamac.statistical.operations.web.client.operation.view.handlers.OperationListUiHandlers;
import org.siemac.metamac.statistical.operations.web.client.utils.RecordUtils;
import org.siemac.metamac.statistical.operations.web.client.widgets.ListGridToolStrip;
import org.siemac.metamac.statistical.operations.web.client.widgets.ModalWindow;
import org.siemac.metamac.statistical.operations.web.client.widgets.NewOperationForm;
import org.siemac.metamac.web.common.client.widgets.CustomListGrid;

import com.google.gwt.user.client.ui.Widget;
import com.google.inject.Inject;
import com.gwtplatform.mvp.client.ViewWithUiHandlers;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.fields.events.HasClickHandlers;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.HasRecordClickHandlers;
import com.smartgwt.client.widgets.grid.events.SelectionChangedHandler;
import com.smartgwt.client.widgets.grid.events.SelectionEvent;
import com.smartgwt.client.widgets.layout.VLayout;

public class OperationListViewImpl extends ViewWithUiHandlers<OperationListUiHandlers> implements OperationListPresenter.OperationListView {

    private VLayout           panel;
    private ListGridToolStrip listGridToolStrip;
    private CustomListGrid    operationListGrid;

    // Modal window
    private ModalWindow       window;
    private NewOperationForm  newOperationForm;

    @Inject
    public OperationListViewImpl() {
        super();
        panel = new VLayout();

        newOperationForm = new NewOperationForm();
        newOperationForm.getSubjectAreasItem().getSchemeItem().addChangedHandler(new ChangedHandler() {

            @Override
            public void onChanged(ChangedEvent event) {
                if (event.getValue() != null) {
                    getUiHandlers().populateSubjects(event.getValue().toString());
                }
            }
        });

        window = new ModalWindow();
        window.setTitle(OperationsWeb.getConstants().actionNewOperation());
        window.setAutoSize(true);
        window.addItem(newOperationForm);

        listGridToolStrip = new ListGridToolStrip(OperationsWeb.getConstants().operationDeleteConfirmation());
        listGridToolStrip.getNewButton().addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {

            @Override
            public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
                getUiHandlers().retrieveCategorySchemes();
                window.show();
            }
        });

        operationListGrid = new CustomListGrid();
        ListGridField codeField = new ListGridField(OperationRecord.CODE, OperationsWeb.getConstants().operationIdentifier());
        ListGridField titleField = new ListGridField(OperationRecord.TITLE, OperationsWeb.getConstants().operation());
        ListGridField descriptionField = new ListGridField(OperationRecord.ACRONYM, OperationsWeb.getConstants().operationAcronym());
        ListGridField statusField = new ListGridField(OperationRecord.STATUS, OperationsWeb.getConstants().operationStatus());
        operationListGrid.setFields(codeField, titleField, descriptionField, statusField);
        operationListGrid.addSelectionChangedHandler(new SelectionChangedHandler() {

            @Override
            public void onSelectionChanged(SelectionEvent event) {
                if (operationListGrid.getSelectedRecords() != null && operationListGrid.getSelectedRecords().length == 1) {
                    OperationRecord record = (OperationRecord) operationListGrid.getSelectedRecord();
                    selectOperation(record.getId());
                } else {
                    // No record selected
                    deselectOperation();
                    if (operationListGrid.getSelectedRecords().length > 1) {
                        // Delete more than one Family with one click
                        listGridToolStrip.getDeleteButton().show();
                    }
                }
            }
        });

        panel.addMember(listGridToolStrip);
        panel.addMember(operationListGrid);
    }

    @Override
    public Widget asWidget() {
        return panel;
    }

    /*
     * GWTP will call setInSlot when a child presenter asks to be added under this view
     */
    @Override
    public void setInSlot(Object slot, Widget content) {
        if (slot == OperationListPresenter.TYPE_SetContextAreaContentToolBar) {
            if (content != null) {
                panel.addMember(content, 0);
            }
        } else {
            // To support inheritance in your views it is good practice to call super.setInSlot when you can't handle the call.
            // Who knows, maybe the parent class knows what to do with this slot.
            super.setInSlot(slot, content);
        }
    }

    @Override
    public void setOperations(List<OperationBaseDto> operationBaseDtos) {
        operationListGrid.removeAllData();
        if (operationBaseDtos != null) {
            for (OperationBaseDto operationBaseDto : operationBaseDtos) {
                operationListGrid.addData(RecordUtils.getOperationRecord(operationBaseDto));
            }
        }
        listGridToolStrip.getDeleteButton().hide();
    }

    @Override
    public HasRecordClickHandlers getSelectedOperation() {
        return operationListGrid;
    }

    @Override
    public HasClickHandlers getSaveNewOperation() {
        return newOperationForm.getSave();
    }

    @Override
    public OperationDto getOperation() {
        return newOperationForm.getOperation();
    }

    @Override
    public boolean validate() {
        return newOperationForm.validate();
    }

    @Override
    public void closeOperationWindow() {
        window.hide();
    }

    @Override
    public com.smartgwt.client.widgets.events.HasClickHandlers getDelete() {
        return listGridToolStrip.getDeleteConfirmationWindow().getYesButton();
    }

    @Override
    public List<Long> getSelectedOperations() {
        List<Long> selectedOperations = new ArrayList<Long>();
        if (operationListGrid.getSelectedRecords() != null) {
            ListGridRecord[] records = operationListGrid.getSelectedRecords();
            for (int i = 0; i < records.length; i++) {
                OperationRecord record = (OperationRecord) records[i];
                selectedOperations.add(record.getId());
            }
        }
        return selectedOperations;
    }

    @Override
    public void onOperationSaved(OperationDto operationDto) {
        OperationRecord record = RecordUtils.getOperationRecord(operationDto);
        operationListGrid.addData(record);
    }

    /**
     * Select Operation in ListGrid
     * 
     * @param id
     */
    private void selectOperation(Long id) {
        if (id == null) {
            // New operation
            listGridToolStrip.getDeleteButton().hide();
            operationListGrid.deselectAllRecords();
        } else {
            listGridToolStrip.getDeleteButton().show();
        }
    }

    /**
     * DeSelect Operation in ListGrid
     */
    private void deselectOperation() {
        listGridToolStrip.getDeleteButton().hide();
    }

    @Override
    public void setCategorySchemes(List<ExternalItemBtDto> schemes) {
        this.newOperationForm.setSubjectAreasSchemes(schemes);
    }

    @Override
    public void setSubjects(List<ExternalItemBtDto> subjects) {
        this.newOperationForm.setSubjetcAreas(subjects);
    }

}
