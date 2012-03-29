package org.siemac.metamac.gopestat.web.client.family.view;

import java.util.ArrayList;
import java.util.List;

import org.siemac.metamac.gopestat.core.dto.serviceapi.FamilyBaseDto;
import org.siemac.metamac.gopestat.core.dto.serviceapi.FamilyDto;
import org.siemac.metamac.gopestat.web.client.GopestatWeb;
import org.siemac.metamac.gopestat.web.client.family.presenter.FamilyListPresenter;
import org.siemac.metamac.gopestat.web.client.family.view.handlers.FamilyListUiHandlers;
import org.siemac.metamac.gopestat.web.client.model.FamilyRecord;
import org.siemac.metamac.gopestat.web.client.utils.RecordUtils;
import org.siemac.metamac.gopestat.web.client.widgets.ListGridToolStrip;
import org.siemac.metamac.gopestat.web.client.widgets.ModalWindow;
import org.siemac.metamac.gopestat.web.client.widgets.NewFamilyForm;
import org.siemac.metamac.web.common.client.widgets.CustomListGrid;

import com.google.gwt.user.client.ui.Widget;
import com.google.inject.Inject;
import com.gwtplatform.mvp.client.ViewWithUiHandlers;
import com.smartgwt.client.widgets.form.fields.events.HasClickHandlers;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.HasRecordClickHandlers;
import com.smartgwt.client.widgets.grid.events.SelectionChangedHandler;
import com.smartgwt.client.widgets.grid.events.SelectionEvent;
import com.smartgwt.client.widgets.layout.VLayout;

public class FamilyListViewImpl extends ViewWithUiHandlers<FamilyListUiHandlers> implements FamilyListPresenter.FamilyListView {

    private VLayout           panel;
    private ListGridToolStrip listGridToolStrip;
    private CustomListGrid    familyListGrid;

    // Modal window
    private ModalWindow       window;
    private NewFamilyForm     newFamilyForm;

    @Inject
    public FamilyListViewImpl() {
        super();
        panel = new VLayout();

        newFamilyForm = new NewFamilyForm();
        listGridToolStrip = new ListGridToolStrip(GopestatWeb.getConstants().familyDeleteConfirmation());
        listGridToolStrip.getNewButton().addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {

            @Override
            public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
                window = new ModalWindow();
                window.setTitle(GopestatWeb.getConstants().actionNewFamily());
                window.setAutoSize(true);
                window.addItem(newFamilyForm);
                window.show();
            }
        });

        familyListGrid = new CustomListGrid();
        ListGridField codeField = new ListGridField(FamilyRecord.CODE, GopestatWeb.getConstants().familyIdentifier());
        ListGridField titleField = new ListGridField(FamilyRecord.TITLE, GopestatWeb.getConstants().familyTitle());
        ListGridField descriptionField = new ListGridField(FamilyRecord.DESCRIPTION, GopestatWeb.getConstants().familyDescription());
        ListGridField statusField = new ListGridField(FamilyRecord.STATUS, GopestatWeb.getConstants().familyStatus());
        familyListGrid.setFields(codeField, titleField, descriptionField, statusField);
        familyListGrid.addSelectionChangedHandler(new SelectionChangedHandler() {

            @Override
            public void onSelectionChanged(SelectionEvent event) {
                if (familyListGrid.getSelectedRecords() != null && familyListGrid.getSelectedRecords().length == 1) {
                    FamilyRecord record = (FamilyRecord) familyListGrid.getSelectedRecord();
                    selectFamily(record.getId());
                } else {
                    // No record selected
                    deselectFamily();
                    if (familyListGrid.getSelectedRecords().length > 1) {
                        // Delete more than one Family with one click
                        listGridToolStrip.getDeleteButton().show();
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
    public void setFamilies(List<FamilyBaseDto> familyDtos) {
        familyListGrid.removeAllData();
        if (familyDtos != null) {
            for (FamilyBaseDto familyDto : familyDtos) {
                familyListGrid.addData(RecordUtils.getFamilyRecord(familyDto));
            }
        }
        listGridToolStrip.getDeleteButton().hide();
    }

    @Override
    public HasRecordClickHandlers getSelectedFamily() {
        return familyListGrid;
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
        window.destroy();
    }

    @Override
    public com.smartgwt.client.widgets.events.HasClickHandlers getDelete() {
        return listGridToolStrip.getDeleteConfirmationWindow().getYesButton();
    }

    @Override
    public List<Long> getSelectedFamilies() {
        List<Long> selectedFamilies = new ArrayList<Long>();
        if (familyListGrid.getSelectedRecords() != null) {
            ListGridRecord[] records = familyListGrid.getSelectedRecords();
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
        familyListGrid.addData(record);
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
            familyListGrid.deselectAllRecords();
        } else {
            listGridToolStrip.getDeleteButton().show();
        }
    }

    /**
     * DeSelect Family in ListGrid
     */
    private void deselectFamily() {
        listGridToolStrip.getDeleteButton().hide();
    }

}
