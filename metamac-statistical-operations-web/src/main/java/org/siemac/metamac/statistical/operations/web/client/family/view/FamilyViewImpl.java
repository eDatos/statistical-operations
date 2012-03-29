package org.siemac.metamac.statistical.operations.web.client.family.view;

import java.util.List;

import org.siemac.metamac.statistical.operations.core.dto.serviceapi.FamilyDto;
import org.siemac.metamac.statistical.operations.core.dto.serviceapi.OperationBaseDto;
import org.siemac.metamac.statistical.operations.web.client.OperationsWeb;
import org.siemac.metamac.statistical.operations.web.client.family.presenter.FamilyPresenter;
import org.siemac.metamac.statistical.operations.web.client.family.view.handlers.FamilyUiHandlers;
import org.siemac.metamac.statistical.operations.web.client.model.OperationRecord;
import org.siemac.metamac.statistical.operations.web.client.resources.GlobalResources;
import org.siemac.metamac.statistical.operations.web.client.utils.RecordUtils;
import org.siemac.metamac.statistical.operations.web.client.widgets.AddOperationsToFamilyForm;
import org.siemac.metamac.statistical.operations.web.client.widgets.ModalWindow;
import org.siemac.metamac.statistical.operations.web.client.widgets.PublishMainFormLayout;
import org.siemac.metamac.web.common.client.utils.InternationalStringUtils;
import org.siemac.metamac.web.common.client.widgets.TitleLabel;
import org.siemac.metamac.web.common.client.widgets.form.GroupDynamicForm;
import org.siemac.metamac.web.common.client.widgets.form.fields.MultiLanguageTextItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.RequiredTextItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.ViewMultiLanguageTextItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.ViewTextItem;

import com.google.gwt.user.client.ui.Widget;
import com.gwtplatform.mvp.client.ViewWithUiHandlers;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.HasClickHandlers;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.events.HasRecordClickHandlers;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

public class FamilyViewImpl extends ViewWithUiHandlers<FamilyUiHandlers> implements FamilyPresenter.FamilyView {

    // IDENTIFIERS
    private static final String       FAMILY_IDENTIFIER              = "id";
    private static final String       FAMILY_URI                     = "uri";
    private static final String       FAMILY_TITLE                   = "titleItem";
    private static final String       FAMILY_ACRONYM                 = "titleItem-alter";
    // CONTENT DESCRIPTORS
    private static final String       FAMILY_DESCRIPTION             = "description";
    // PRODUCTION DESCRIPTORS
    private static final String       FAMILY_INTERNAL_INVENTORY_DATE = "internal-inv";
    private static final String       FAMILY_STATUS                  = "status";
    // DIFFUSION
    private static final String       FAMILY_INVENTORY_DATE          = "inv";

    private VLayout                   panel;

    private PublishMainFormLayout     mainFormLayout;
    private GroupDynamicForm          familyViewForm;
    private GroupDynamicForm          familyEditionForm;

    private MultiLanguageTextItem     titleItem;
    private MultiLanguageTextItem     acronymItem;
    private MultiLanguageTextItem     descriptionItem;

    private ToolStrip                 operationToolStrip;
    private ToolStripButton           editToolStripButton;
    private ListGrid                  operationListGrid;

    // Add operations to family modal
    private ModalWindow               window;
    private AddOperationsToFamilyForm addOperationsToFamilyForm;

    private List<OperationBaseDto>    operationDtos;
    private List<OperationBaseDto>    allOperations;

    public FamilyViewImpl() {
        super();
        panel = new VLayout();

        // Family

        mainFormLayout = new PublishMainFormLayout();
        mainFormLayout.getTranslateToolStripButton().addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                setTranslationsShowed(mainFormLayout.getTranslateToolStripButton().isSelected());
            }
        });
        createViewForm();
        createEditionForm();

        // Operations

        addOperationsToFamilyForm = new AddOperationsToFamilyForm();
        operationToolStrip = new ToolStrip();
        operationToolStrip.setWidth100();
        editToolStripButton = new ToolStripButton(OperationsWeb.getConstants().actionEdit(), GlobalResources.RESOURCE.editListGrid().getURL());
        editToolStripButton.addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                window = new ModalWindow();
                window.setTitle(OperationsWeb.getConstants().actionAddOperationsToFamily());
                window.setAutoSize(true);
                addOperationsToFamilyForm.clearValues();
                addOperationsToFamilyForm.setOperationsValueMap(allOperations);
                addOperationsToFamilyForm.setOperations(operationDtos);
                window.addItem(addOperationsToFamilyForm);
                window.show();
            }
        });
        operationToolStrip.addButton(editToolStripButton);

        TitleLabel operationsTitleLabel = new TitleLabel(OperationsWeb.getConstants().operations());
        operationsTitleLabel.setStyleName("sectionTitleLeftMargin");

        operationListGrid = new ListGrid();
        operationListGrid.setHeight(150);
        ListGridField identifierField = new ListGridField(OperationRecord.ID, OperationsWeb.getConstants().familyIdentifier());
        ListGridField titleField = new ListGridField(OperationRecord.TITLE, OperationsWeb.getConstants().familyTitle());
        ListGridField titleAlternativeField = new ListGridField(OperationRecord.ACRONYM, OperationsWeb.getConstants().familyAcronym());
        ListGridField statusField = new ListGridField(OperationRecord.STATUS, OperationsWeb.getConstants().operationStatus());
        operationListGrid.setFields(identifierField, titleField, titleAlternativeField, statusField);

        VLayout operationsLayout = new VLayout();
        operationsLayout.setMargin(15);
        operationsLayout.addMember(operationToolStrip);
        operationsLayout.addMember(operationListGrid);

        panel.addMember(mainFormLayout);
        panel.addMember(operationsTitleLabel);
        panel.addMember(operationsLayout);
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
        if (slot == FamilyPresenter.TYPE_SetContextAreaContentToolBar) {
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
    public void setFamily(FamilyDto familyDto, List<OperationBaseDto> operationDtos) {
        // Set Family
        setFamily(familyDto);

        // Set Operations
        setOperations(operationDtos);
    }

    @Override
    public FamilyDto getFamily(FamilyDto familyDto) {
        familyDto.setCode(familyEditionForm.getValueAsString(FAMILY_IDENTIFIER));
        familyDto.setTitle(titleItem.getValue());
        familyDto.setAcronym(acronymItem.getValue());
        familyDto.setDescription(descriptionItem.getValue());
        return familyDto;
    }

    @Override
    public void setAllOperations(List<OperationBaseDto> operationDtos) {
        allOperations = operationDtos;
    }

    private void setViewForm(FamilyDto familyDto) {
        familyViewForm.setValue(FAMILY_IDENTIFIER, familyDto.getCode());
        familyViewForm.setValue(FAMILY_URI, familyDto.getUri());
        familyViewForm.setValue(FAMILY_TITLE, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(familyDto.getTitle()));
        familyViewForm.setValue(FAMILY_ACRONYM, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(familyDto.getAcronym()));
        familyViewForm.setValue(FAMILY_DESCRIPTION, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(familyDto.getDescription()));
        familyViewForm.setValue(FAMILY_INTERNAL_INVENTORY_DATE, familyDto.getInternalInventoryDate() == null ? "" : familyDto.getInternalInventoryDate().toString());
        familyViewForm.setValue(FAMILY_STATUS, OperationsWeb.getCoreMessages().getString(OperationsWeb.getCoreMessages().procStatusEnum() + familyDto.getProcStatus().getName()));
        familyViewForm.setValue(FAMILY_INVENTORY_DATE, familyDto.getInventoryDate() == null ? "" : familyDto.getInventoryDate().toString());
    }

    private void setEditionForm(FamilyDto familyDto) {
        familyEditionForm.setValue(FAMILY_IDENTIFIER, familyDto.getCode());
        familyEditionForm.setValue(FAMILY_URI, familyDto.getUri());
        familyEditionForm.setValue(FAMILY_TITLE, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(familyDto.getTitle()));
        familyEditionForm.setValue(FAMILY_ACRONYM, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(familyDto.getAcronym()));
        familyEditionForm.setValue(FAMILY_DESCRIPTION, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(familyDto.getDescription()));
        familyEditionForm.setValue(FAMILY_INTERNAL_INVENTORY_DATE, familyDto.getInternalInventoryDate() == null ? "" : familyDto.getInternalInventoryDate().toString());
        familyEditionForm.setValue(FAMILY_STATUS, OperationsWeb.getCoreMessages().getString(OperationsWeb.getCoreMessages().procStatusEnum() + familyDto.getProcStatus().getName()));
        familyEditionForm.setValue(FAMILY_INVENTORY_DATE, familyDto.getInventoryDate() == null ? "" : familyDto.getInventoryDate().toString());
    }

    @Override
    public void setOperations(List<OperationBaseDto> operationBaseDtos) {
        this.operationDtos = operationBaseDtos;
        // Set operations in listGrid
        operationListGrid.selectAllRecords();
        operationListGrid.removeSelectedData();
        operationListGrid.deselectAllRecords();
        if (operationBaseDtos != null) {
            for (OperationBaseDto operationBaseDto : operationBaseDtos) {
                operationListGrid.addData(RecordUtils.getOperationRecord(operationBaseDto));
            }
        }
    }

    @Override
    public HasRecordClickHandlers getSelectedOperation() {
        return operationListGrid;
    }

    @Override
    public boolean validate() {
        return familyEditionForm.validate(false);
    }

    private void createViewForm() {
        // Family Form
        familyViewForm = new GroupDynamicForm(OperationsWeb.getConstants().family());
        ViewTextItem code = new ViewTextItem(FAMILY_IDENTIFIER, OperationsWeb.getConstants().familyIdentifier());
        ViewTextItem uri = new ViewTextItem(FAMILY_URI, OperationsWeb.getConstants().familyUri());
        ViewMultiLanguageTextItem title = new ViewMultiLanguageTextItem(FAMILY_TITLE, OperationsWeb.getConstants().familyTitle());
        ViewMultiLanguageTextItem acronym = new ViewMultiLanguageTextItem(FAMILY_ACRONYM, OperationsWeb.getConstants().familyAcronym());
        ViewMultiLanguageTextItem description = new ViewMultiLanguageTextItem(FAMILY_DESCRIPTION, OperationsWeb.getConstants().familyDescription());
        ViewTextItem internalInventoryDate = new ViewTextItem(FAMILY_INTERNAL_INVENTORY_DATE, OperationsWeb.getConstants().familyInternalInventoryDate());
        ViewTextItem status = new ViewTextItem(FAMILY_STATUS, OperationsWeb.getConstants().familyStatus());
        ViewTextItem inventoryDate = new ViewTextItem(FAMILY_INVENTORY_DATE, OperationsWeb.getConstants().familyInventoryDate());
        familyViewForm.setFields(code, uri, title, acronym, description, internalInventoryDate, status, inventoryDate);
        // Add to main layout
        mainFormLayout.addViewCanvas(familyViewForm);
    }

    private void createEditionForm() {
        // Family Form
        familyEditionForm = new GroupDynamicForm(OperationsWeb.getConstants().family());
        RequiredTextItem code = new RequiredTextItem(FAMILY_IDENTIFIER, OperationsWeb.getConstants().familyCode());
        ViewTextItem uri = new ViewTextItem(FAMILY_URI, OperationsWeb.getConstants().familyUri());
        titleItem = new MultiLanguageTextItem(FAMILY_TITLE, OperationsWeb.getConstants().familyTitle());
        titleItem.setRequired(true);
        acronymItem = new MultiLanguageTextItem(FAMILY_ACRONYM, OperationsWeb.getConstants().familyAcronym());
        descriptionItem = new MultiLanguageTextItem(FAMILY_DESCRIPTION, OperationsWeb.getConstants().familyDescription());
        ViewTextItem internalInventoryDate = new ViewTextItem(FAMILY_INTERNAL_INVENTORY_DATE, OperationsWeb.getConstants().familyInternalInventoryDate());
        ViewTextItem status = new ViewTextItem(FAMILY_STATUS, OperationsWeb.getConstants().familyStatus());
        ViewTextItem inventoryDate = new ViewTextItem(FAMILY_INVENTORY_DATE, OperationsWeb.getConstants().familyInventoryDate());
        familyEditionForm.setFields(code, uri, titleItem, acronymItem, descriptionItem, internalInventoryDate, status, inventoryDate);
        // Add to main layout
        mainFormLayout.addEditionCanvas(familyEditionForm);
    }

    @Override
    public HasClickHandlers getSave() {
        return mainFormLayout.getSave();
    }

    @Override
    public void onFamilySaved(FamilyDto familyDto) {
        setFamily(familyDto);
    }

    @Override
    public com.smartgwt.client.widgets.form.fields.events.HasClickHandlers getAddOperations() {
        return addOperationsToFamilyForm.getAdd();
    }

    @Override
    public List<Long> getSelectedOperationIds() {
        return addOperationsToFamilyForm.getSelectedOperationIds();
    }

    @Override
    public boolean validateAddOperations() {
        return addOperationsToFamilyForm.validate();
    }

    @Override
    public void closeOperationsWindow() {
        window.destroy();
    }

    private void setFamily(FamilyDto familyDto) {
        mainFormLayout.setViewMode();
        mainFormLayout.updatePublishSection(familyDto.getProcStatus());
        // Set Family
        mainFormLayout.setTitleLabelContents(InternationalStringUtils.getLocalisedString(familyDto.getTitle()));
        // Form
        setViewForm(familyDto);
        setEditionForm(familyDto);
    }

    @Override
    public HasClickHandlers getPublishFamilyInternally() {
        return mainFormLayout.getPublishInternally();
    }

    @Override
    public HasClickHandlers getPublishFamilyExternally() {
        return mainFormLayout.getPublishExternally();
    }

    private void setTranslationsShowed(boolean translationsShowed) {
        // Set translationsShowed value to international fields
        familyViewForm.setTranslationsShowed(translationsShowed);
        familyEditionForm.setTranslationsShowed(translationsShowed);
    }

}
