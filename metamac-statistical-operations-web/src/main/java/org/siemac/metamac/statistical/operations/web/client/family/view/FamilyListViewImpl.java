package org.siemac.metamac.statistical.operations.web.client.family.view;

import java.util.ArrayList;
import java.util.List;

import org.siemac.metamac.statistical.operations.core.dto.FamilyBaseDto;
import org.siemac.metamac.statistical.operations.core.dto.FamilyDto;
import org.siemac.metamac.statistical.operations.web.client.OperationsWeb;
import org.siemac.metamac.statistical.operations.web.client.family.presenter.FamilyListPresenter;
import org.siemac.metamac.statistical.operations.web.client.family.view.handlers.FamilyListUiHandlers;
import org.siemac.metamac.statistical.operations.web.client.model.FamilyRecord;
import org.siemac.metamac.statistical.operations.web.client.model.ds.FamilyDS;
import org.siemac.metamac.statistical.operations.web.client.utils.ClientSecurityUtils;
import org.siemac.metamac.statistical.operations.web.client.utils.RecordUtils;
import org.siemac.metamac.statistical.operations.web.client.widgets.ListGridToolStrip;
import org.siemac.metamac.statistical.operations.web.client.widgets.ModalWindow;
import org.siemac.metamac.statistical.operations.web.client.widgets.NewFamilyForm;
import org.siemac.metamac.web.common.client.widgets.PaginatedCheckListGrid;
import org.siemac.metamac.web.common.client.widgets.actions.PaginatedAction;

import com.google.gwt.user.client.ui.Widget;
import com.google.inject.Inject;
import com.gwtplatform.mvp.client.ViewWithUiHandlers;
import com.smartgwt.client.types.Visibility;
import com.smartgwt.client.widgets.form.fields.events.HasClickHandlers;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.HasRecordClickHandlers;
import com.smartgwt.client.widgets.grid.events.SelectionChangedHandler;
import com.smartgwt.client.widgets.grid.events.SelectionEvent;
import com.smartgwt.client.widgets.layout.VLayout;

public class FamilyListViewImpl extends ViewWithUiHandlers<FamilyListUiHandlers> implements FamilyListPresenter.FamilyListView {

    private VLayout                panel;
    private ListGridToolStrip      listGridToolStrip;
    private PaginatedCheckListGrid familyListGrid;

    // Modal window
    private ModalWindow            window;
    private NewFamilyForm          newFamilyForm;

    @Inject
    public FamilyListViewImpl() {
        super();
        panel = new VLayout();

        newFamilyForm = new NewFamilyForm();

        window = new ModalWindow();
        window.setTitle(OperationsWeb.getConstants().actionNewFamily());
        window.setAutoSize(true);
        window.addItem(newFamilyForm);

        listGridToolStrip = new ListGridToolStrip(OperationsWeb.getConstants().familyDeleteConfirmation());
        listGridToolStrip.getNewButton().addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {

            @Override
            public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
                // Clear new family form
                newFamilyForm.clearValues();

                window.show();
            }
        });
        listGridToolStrip.getNewButton().setVisibility(ClientSecurityUtils.canCreateFamily() ? Visibility.VISIBLE : Visibility.HIDDEN);

        familyListGrid = new PaginatedCheckListGrid(FamilyListPresenter.FAMILY_LIST_MAX_RESULTS, new PaginatedAction() {

            @Override
            public void retrieveResultSet(int firstResult, int maxResults) {
                getUiHandlers().retrieveFamilyList(firstResult, maxResults);
            }
        });
        familyListGrid.setHeight(720);
        ListGridField codeField = new ListGridField(FamilyDS.CODE, OperationsWeb.getConstants().familyIdentifier());
        ListGridField titleField = new ListGridField(FamilyDS.TITLE, OperationsWeb.getConstants().familyTitle());
        ListGridField descriptionField = new ListGridField(FamilyDS.DESCRIPTION, OperationsWeb.getConstants().familyDescription());
        ListGridField statusField = new ListGridField(FamilyDS.PROC_STATUS, OperationsWeb.getConstants().familyProcStatus());
        familyListGrid.getListGrid().setFields(codeField, titleField, descriptionField, statusField);
        familyListGrid.getListGrid().addSelectionChangedHandler(new SelectionChangedHandler() {

            @Override
            public void onSelectionChanged(SelectionEvent event) {
                if (familyListGrid.getListGrid().getSelectedRecords() != null && familyListGrid.getListGrid().getSelectedRecords().length == 1) {
                    FamilyRecord record = (FamilyRecord) familyListGrid.getListGrid().getSelectedRecord();
                    selectFamily(record.getId());
                } else {
                    // No record selected
                    deselectFamily();
                    if (familyListGrid.getListGrid().getSelectedRecords().length > 1) {
                        // Delete more than one Family with one click
                        showListGridDeleteButton();
                    }
                }
            }
        });

        panel.addMember(listGridToolStrip);
        panel.addMember(familyListGrid);
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
        if (slot == FamilyListPresenter.TYPE_SetContextAreaContentToolBar) {
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
    public void setFamilies(List<FamilyBaseDto> familyDtos, int firstResult, int totalResults) {
        familyListGrid.getListGrid().selectAllRecords();
        familyListGrid.getListGrid().removeSelectedData();
        if (familyDtos != null) {
            for (FamilyBaseDto familyDto : familyDtos) {
                familyListGrid.getListGrid().addData(RecordUtils.getFamilyRecord(familyDto));
            }
        }
        familyListGrid.refreshPaginationInfo(firstResult, familyDtos.size(), totalResults);
        listGridToolStrip.getDeleteButton().hide();
    }

    @Override
    public HasRecordClickHandlers getSelectedFamily() {
        return familyListGrid.getListGrid();
    }

    @Override
    public HasClickHandlers getSaveNewFamily() {
        return newFamilyForm.getSave();
    }

    @Override
    public FamilyDto getFamily() {
        return newFamilyForm.getFamily();
    }

    @Override
    public boolean validate() {
        return newFamilyForm.validate();
    }

    @Override
    public void closeFamilyWindow() {
        window.hide();
    }

    @Override
    public com.smartgwt.client.widgets.events.HasClickHandlers getDelete() {
        return listGridToolStrip.getDeleteConfirmationWindow().getYesButton();
    }

    @Override
    public List<Long> getSelectedFamilies() {
        List<Long> selectedFamilies = new ArrayList<Long>();
        if (familyListGrid.getListGrid().getSelectedRecords() != null) {
            ListGridRecord[] records = familyListGrid.getListGrid().getSelectedRecords();
            for (int i = 0; i < records.length; i++) {
                FamilyRecord record = (FamilyRecord) records[i];
                selectedFamilies.add(record.getId());
            }
        }
        return selectedFamilies;
    }

    @Override
    public void onFamilySaved(FamilyDto familyDto) {
        FamilyRecord record = RecordUtils.getFamilyRecord(familyDto);
        familyListGrid.getListGrid().addData(record);
        familyListGrid.goToLastPageAfterCreate();
    }

    /**
     * Select Family in ListGrid
     * 
     * @param id
     */
    private void selectFamily(Long id) {
        if (id == null) {
            // New family
            listGridToolStrip.getDeleteButton().hide();
            familyListGrid.getListGrid().deselectAllRecords();
        } else {
            showListGridDeleteButton();
        }
    }

    /**
     * DeSelect Family in ListGrid
     */
    private void deselectFamily() {
        listGridToolStrip.getDeleteButton().hide();
    }

    private void showListGridDeleteButton() {
        if (ClientSecurityUtils.canDeleteFamily()) {
            listGridToolStrip.getDeleteButton().show();
        }
    }

}
