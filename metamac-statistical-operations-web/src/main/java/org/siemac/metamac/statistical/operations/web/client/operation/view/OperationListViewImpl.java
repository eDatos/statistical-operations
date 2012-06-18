package org.siemac.metamac.statistical.operations.web.client.operation.view;

import java.util.ArrayList;
import java.util.List;

import org.siemac.metamac.core.common.dto.ExternalItemBtDto;
import org.siemac.metamac.domain.statistical.operations.dto.OperationBaseDto;
import org.siemac.metamac.domain.statistical.operations.dto.OperationDto;
import org.siemac.metamac.statistical.operations.web.client.OperationsWeb;
import org.siemac.metamac.statistical.operations.web.client.model.OperationRecord;
import org.siemac.metamac.statistical.operations.web.client.model.ds.OperationDS;
import org.siemac.metamac.statistical.operations.web.client.operation.presenter.OperationListPresenter;
import org.siemac.metamac.statistical.operations.web.client.operation.view.handlers.OperationListUiHandlers;
import org.siemac.metamac.statistical.operations.web.client.utils.ClientSecurityUtils;
import org.siemac.metamac.statistical.operations.web.client.utils.RecordUtils;
import org.siemac.metamac.statistical.operations.web.client.widgets.ListGridToolStrip;
import org.siemac.metamac.statistical.operations.web.client.widgets.ModalWindow;
import org.siemac.metamac.statistical.operations.web.client.widgets.NewOperationForm;
import org.siemac.metamac.web.common.client.widgets.PaginatedBaseCustomListGrid;
import org.siemac.metamac.web.common.client.widgets.actions.PaginatedAction;

import com.google.gwt.user.client.ui.Widget;
import com.google.inject.Inject;
import com.gwtplatform.mvp.client.ViewWithUiHandlers;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.Visibility;
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

    private VLayout                     panel;
    private ListGridToolStrip           listGridToolStrip;
    private PaginatedBaseCustomListGrid operationListGrid;

    // Modal window
    private ModalWindow                 window;
    private NewOperationForm            newOperationForm;

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
                // Load category schemes (required to create a new operation)
                getUiHandlers().retrieveCategorySchemes();

                // Clear new operation form
                newOperationForm.clearValues();

                window.show();
            }
        });
        listGridToolStrip.getNewButton().setVisibility(ClientSecurityUtils.canCreateOperation() ? Visibility.VISIBLE : Visibility.HIDDEN);

        operationListGrid = new PaginatedBaseCustomListGrid(OperationListPresenter.OPERATION_LIST_MAX_RESULTS, new PaginatedAction() {

            @Override
            public void retrieveResultSet(int firstResult, int maxResults) {
                getUiHandlers().retrieveOperationList(firstResult, maxResults);
            }
        });
        ListGridField codeField = new ListGridField(OperationDS.OP_CODE, OperationsWeb.getConstants().operationIdentifier());
        ListGridField titleField = new ListGridField(OperationDS.OP_TITLE, OperationsWeb.getConstants().operationTitle());
        ListGridField descriptionField = new ListGridField(OperationDS.OP_ACRONYM, OperationsWeb.getConstants().operationAcronym());
        ListGridField statusField = new ListGridField(OperationDS.OP_PROC_STATUS, OperationsWeb.getConstants().operationStatus());
        ListGridField indicatorsSystem = new ListGridField(OperationDS.OP_INDICATOR_SYSTEM, OperationsWeb.getConstants().operationIndicatorSystem());
        indicatorsSystem.setType(ListGridFieldType.IMAGE);
        operationListGrid.getListGrid().setFields(codeField, titleField, descriptionField, statusField, indicatorsSystem);
        operationListGrid.getListGrid().addSelectionChangedHandler(new SelectionChangedHandler() {

            @Override
            public void onSelectionChanged(SelectionEvent event) {
                if (operationListGrid.getListGrid().getSelectedRecords() != null && operationListGrid.getListGrid().getSelectedRecords().length == 1) {
                    OperationRecord record = (OperationRecord) operationListGrid.getListGrid().getSelectedRecord();
                    selectOperation(record.getId());
                } else {
                    // No record selected
                    deselectOperation();
                    if (operationListGrid.getListGrid().getSelectedRecords().length > 1) {
                        // Delete more than one Family with one click
                        showListGridDeleteButton();
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
    public void setOperations(List<OperationBaseDto> operationBaseDtos, int firstResult, int totalResults) {
        operationListGrid.getListGrid().selectAllRecords();
        operationListGrid.getListGrid().removeSelectedData();
        if (operationBaseDtos != null) {
            for (OperationBaseDto operationBaseDto : operationBaseDtos) {
                operationListGrid.getListGrid().addData(RecordUtils.getOperationRecord(operationBaseDto));
            }
        }
        operationListGrid.refreshPaginationInfo(firstResult, operationBaseDtos.size(), totalResults);
        listGridToolStrip.getDeleteButton().hide();
    }

    @Override
    public HasRecordClickHandlers getSelectedOperation() {
        return operationListGrid.getListGrid();
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
        if (operationListGrid.getListGrid().getSelectedRecords() != null) {
            ListGridRecord[] records = operationListGrid.getListGrid().getSelectedRecords();
            for (int i = 0; i < records.length; i++) {
                OperationRecord record = (OperationRecord) records[i];
                selectedOperations.add(record.getId());
            }
        }
        return selectedOperations;
    }

    public List<String> getSelectedOperationCodes() {
        List<String> selectedOperations = new ArrayList<String>();
        if (operationListGrid.getListGrid().getSelectedRecords() != null) {
            ListGridRecord[] records = operationListGrid.getListGrid().getSelectedRecords();
            for (int i = 0; i < records.length; i++) {
                OperationRecord record = (OperationRecord) records[i];
                selectedOperations.add(record.getCode());
            }
        }
        return selectedOperations;
    }

    @Override
    public void onOperationSaved(OperationDto operationDto) {
        OperationRecord record = RecordUtils.getOperationRecord(operationDto);
        operationListGrid.getListGrid().addData(record);
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
            operationListGrid.getListGrid().deselectAllRecords();
        } else {
            showListGridDeleteButton();
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

    private void showListGridDeleteButton() {
        List<String> selectedOperationCodes = getSelectedOperationCodes();
        boolean actionAllowed = true;
        for (String code : selectedOperationCodes) {
            if (!ClientSecurityUtils.canDeleteOperation(code)) {
                actionAllowed = false;
                break;
            }
        }
        if (actionAllowed) {
            listGridToolStrip.getDeleteButton().show();
        }
    }

}
