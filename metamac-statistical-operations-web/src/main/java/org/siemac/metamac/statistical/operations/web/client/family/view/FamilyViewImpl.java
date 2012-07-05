package org.siemac.metamac.statistical.operations.web.client.family.view;

import java.util.List;

import org.siemac.metamac.statistical.operations.core.dto.FamilyDto;
import org.siemac.metamac.statistical.operations.core.dto.OperationBaseDto;
import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.web.client.OperationsWeb;
import org.siemac.metamac.statistical.operations.web.client.family.presenter.FamilyPresenter;
import org.siemac.metamac.statistical.operations.web.client.family.view.handlers.FamilyUiHandlers;
import org.siemac.metamac.statistical.operations.web.client.model.ds.FamilyDS;
import org.siemac.metamac.statistical.operations.web.client.model.ds.OperationDS;
import org.siemac.metamac.statistical.operations.web.client.resources.GlobalResources;
import org.siemac.metamac.statistical.operations.web.client.utils.ClientSecurityUtils;
import org.siemac.metamac.statistical.operations.web.client.utils.RecordUtils;
import org.siemac.metamac.statistical.operations.web.client.widgets.AddOperationsToFamilyWindow;
import org.siemac.metamac.statistical.operations.web.client.widgets.FamilyMainFormLayout;
import org.siemac.metamac.web.common.client.utils.CommonWebUtils;
import org.siemac.metamac.web.common.client.utils.FormItemUtils;
import org.siemac.metamac.web.common.client.utils.InternationalStringUtils;
import org.siemac.metamac.web.common.client.widgets.TitleLabel;
import org.siemac.metamac.web.common.client.widgets.form.GroupDynamicForm;
import org.siemac.metamac.web.common.client.widgets.form.fields.MultiLanguageTextAreaItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.MultiLanguageTextItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.RequiredTextItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.ViewMultiLanguageTextItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.ViewTextItem;

import com.google.gwt.user.client.ui.Widget;
import com.gwtplatform.mvp.client.ViewWithUiHandlers;
import com.smartgwt.client.types.Visibility;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.HasClickHandlers;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.FormItemIfFunction;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.events.HasRecordClickHandlers;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

public class FamilyViewImpl extends ViewWithUiHandlers<FamilyUiHandlers> implements FamilyPresenter.FamilyView {

    public static final int             OPERATION_LIST_MAX_RESULTS = 17;

    private FamilyUiHandlers            uiHandlers;

    private VLayout                     panel;

    private FamilyMainFormLayout        mainFormLayout;
    private GroupDynamicForm            familyViewForm;
    private GroupDynamicForm            familyEditionForm;

    private MultiLanguageTextItem       titleItem;
    private MultiLanguageTextItem       acronymItem;
    private MultiLanguageTextAreaItem   descriptionItem;

    private ToolStrip                   operationToolStrip;
    private ToolStripButton             editToolStripButton;
    private ListGrid                    operationListGrid;

    private List<OperationBaseDto>      operationBaseDtos;

    // Add operations to family modal
    private AddOperationsToFamilyWindow addOperationsToFamilyWindow;

    public FamilyViewImpl() {
        super();
        panel = new VLayout();

        // Family

        mainFormLayout = new FamilyMainFormLayout(ClientSecurityUtils.canUpdateFamily());
        mainFormLayout.getTranslateToolStripButton().addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                setTranslationsShowed(mainFormLayout.getTranslateToolStripButton().isSelected());
            }
        });
        createViewForm();
        createEditionForm();

        // Operations

        operationToolStrip = new ToolStrip();
        operationToolStrip.setWidth100();
        editToolStripButton = new ToolStripButton(OperationsWeb.getConstants().actionEdit(), GlobalResources.RESOURCE.editListGrid().getURL());
        editToolStripButton.addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                // Load family operations
                uiHandlers.retrievePaginatedOperations(0, OPERATION_LIST_MAX_RESULTS, null);

                addOperationsToFamilyWindow = new AddOperationsToFamilyWindow(uiHandlers);
                addOperationsToFamilyWindow.setSelectedOperations(operationBaseDtos);
            }
        });
        editToolStripButton.setVisibility(ClientSecurityUtils.canAddOperationToFamily() ? Visibility.VISIBLE : Visibility.HIDDEN);
        operationToolStrip.addButton(editToolStripButton);

        TitleLabel operationsTitleLabel = new TitleLabel(OperationsWeb.getConstants().operations());
        operationsTitleLabel.setStyleName("sectionTitleLeftMargin");

        operationListGrid = new ListGrid();
        operationListGrid.setHeight(300);
        ListGridField identifierField = new ListGridField(OperationDS.OP_CODE, OperationsWeb.getConstants().familyIdentifier());
        ListGridField titleField = new ListGridField(OperationDS.OP_TITLE, OperationsWeb.getConstants().familyTitle());
        ListGridField titleAlternativeField = new ListGridField(OperationDS.OP_ACRONYM, OperationsWeb.getConstants().familyAcronym());
        ListGridField statusField = new ListGridField(OperationDS.OP_PROC_STATUS, OperationsWeb.getConstants().operationStatus());
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
    public FamilyDto getFamily(FamilyDto familyDto) {
        familyDto.setCode(familyEditionForm.getValueAsString(FamilyDS.CODE));
        familyDto.setTitle(titleItem.getValue());
        familyDto.setAcronym(acronymItem.getValue());
        familyDto.setDescription(descriptionItem.getValue());
        return familyDto;
    }

    @Override
    public void setOperations(List<OperationBaseDto> operations, int firstResult, int totalResults) {
        addOperationsToFamilyWindow.setOperations(operations, firstResult, totalResults);
    }

    private void setViewForm(FamilyDto familyDto) {
        familyViewForm.setValue(FamilyDS.CODE, familyDto.getCode());
        familyViewForm.setValue(FamilyDS.TITLE, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(familyDto.getTitle()));
        familyViewForm.setValue(FamilyDS.ACRONYM, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(familyDto.getAcronym()));
        familyViewForm.setValue(FamilyDS.DESCRIPTION, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(familyDto.getDescription()));
        familyViewForm.setValue(FamilyDS.INTERNAL_INVENTORY_DATE, familyDto.getInternalInventoryDate());
        familyViewForm.setValue(FamilyDS.PROC_STATUS, OperationsWeb.getCoreMessages().getString(OperationsWeb.getCoreMessages().procStatusEnum() + familyDto.getProcStatus().getName()));
        familyViewForm.setValue(FamilyDS.INVENTORY_DATE, familyDto.getInventoryDate());
    }

    private void setEditionForm(FamilyDto familyDto) {
        familyEditionForm.setValue(FamilyDS.CODE, familyDto.getCode());
        familyEditionForm.setValue(FamilyDS.CODE_VIEW, familyDto.getCode());
        familyEditionForm.setValue(FamilyDS.TITLE, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(familyDto.getTitle()));
        familyEditionForm.setValue(FamilyDS.ACRONYM, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(familyDto.getAcronym()));
        familyEditionForm.setValue(FamilyDS.DESCRIPTION, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(familyDto.getDescription()));
        familyEditionForm.setValue(FamilyDS.INTERNAL_INVENTORY_DATE, familyDto.getInternalInventoryDate());
        familyEditionForm.setValue(FamilyDS.PROC_STATUS, OperationsWeb.getCoreMessages().getString(OperationsWeb.getCoreMessages().procStatusEnum() + familyDto.getProcStatus().getName()));
        familyEditionForm.setValue(FamilyDS.PROC_STATUS_VIEW, familyDto.getProcStatus().toString());
        familyEditionForm.setValue(FamilyDS.INVENTORY_DATE, familyDto.getInventoryDate());
        familyEditionForm.markForRedraw();
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
        ViewTextItem code = new ViewTextItem(FamilyDS.CODE, OperationsWeb.getConstants().familyIdentifier());
        ViewMultiLanguageTextItem title = new ViewMultiLanguageTextItem(FamilyDS.TITLE, OperationsWeb.getConstants().familyTitle());
        ViewMultiLanguageTextItem acronym = new ViewMultiLanguageTextItem(FamilyDS.ACRONYM, OperationsWeb.getConstants().familyAcronym());
        ViewMultiLanguageTextItem description = new ViewMultiLanguageTextItem(FamilyDS.DESCRIPTION, OperationsWeb.getConstants().familyDescription());
        ViewTextItem internalInventoryDate = new ViewTextItem(FamilyDS.INTERNAL_INVENTORY_DATE, OperationsWeb.getConstants().familyInternalInventoryDate());
        ViewTextItem procStatus = new ViewTextItem(FamilyDS.PROC_STATUS, OperationsWeb.getConstants().familyProcStatus());
        ViewTextItem inventoryDate = new ViewTextItem(FamilyDS.INVENTORY_DATE, OperationsWeb.getConstants().familyInventoryDate());
        familyViewForm.setFields(code, title, acronym, procStatus, description, internalInventoryDate, inventoryDate);
        // Add to main layout
        mainFormLayout.addViewCanvas(familyViewForm);
    }

    private void createEditionForm() {
        // Family Form
        familyEditionForm = new GroupDynamicForm(OperationsWeb.getConstants().family());

        // Code
        RequiredTextItem code = new RequiredTextItem(FamilyDS.CODE, OperationsWeb.getConstants().familyIdentifier());
        code.setShowIfCondition(new FormItemIfFunction() {

            @Override
            public boolean execute(FormItem item, Object value, DynamicForm form) {
                return canFamilyCodeBeEdited(form);
            }
        });
        code.setValidators(CommonWebUtils.getSemanticIdentifierCustomValidator());

        ViewTextItem staticCode = new ViewTextItem(FamilyDS.CODE_VIEW, OperationsWeb.getConstants().familyIdentifier());
        staticCode.setShowIfCondition(new FormItemIfFunction() {

            @Override
            public boolean execute(FormItem item, Object value, DynamicForm form) {
                return !canFamilyCodeBeEdited(form);
            }
        });

        titleItem = new MultiLanguageTextItem(FamilyDS.TITLE, OperationsWeb.getConstants().familyTitle());
        titleItem.setRequired(true);
        acronymItem = new MultiLanguageTextItem(FamilyDS.ACRONYM, OperationsWeb.getConstants().familyAcronym());
        descriptionItem = new MultiLanguageTextAreaItem(FamilyDS.DESCRIPTION, OperationsWeb.getConstants().familyDescription());
        ViewTextItem internalInventoryDate = new ViewTextItem(FamilyDS.INTERNAL_INVENTORY_DATE, OperationsWeb.getConstants().familyInternalInventoryDate());

        // Status
        ViewTextItem procStatus = new ViewTextItem(FamilyDS.PROC_STATUS, OperationsWeb.getConstants().familyProcStatus());
        ViewTextItem staticProcStatus = new ViewTextItem(FamilyDS.PROC_STATUS_VIEW, OperationsWeb.getConstants().familyProcStatus());
        staticProcStatus.setShowIfCondition(FormItemUtils.getFalseFormItemIfFunction());

        ViewTextItem inventoryDate = new ViewTextItem(FamilyDS.INVENTORY_DATE, OperationsWeb.getConstants().familyInventoryDate());
        familyEditionForm.setFields(staticCode, code, titleItem, acronymItem, procStatus, descriptionItem, internalInventoryDate, staticProcStatus, inventoryDate);
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
    public void setFamily(FamilyDto familyDto, List<OperationBaseDto> operationBaseDtos) {
        // Family
        setFamily(familyDto);

        // Operations
        setFamilyOperations(operationBaseDtos);
    }

    private void setFamily(FamilyDto familyDto) {
        mainFormLayout.setViewMode();
        mainFormLayout.updatePublishSection(familyDto.getProcStatus());
        mainFormLayout.setTitleLabelContents(InternationalStringUtils.getLocalisedString(familyDto.getTitle()));
        setViewForm(familyDto);
        setEditionForm(familyDto);
    }

    private void setFamilyOperations(List<OperationBaseDto> operationBaseDtos) {
        this.operationBaseDtos = operationBaseDtos;
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

    private boolean canFamilyCodeBeEdited(DynamicForm form) {
        // Family code can be edited only when ProcStatus is DRAFT
        return (form.getValue(FamilyDS.PROC_STATUS_VIEW) != null && ProcStatusEnum.DRAFT.toString().equals(form.getValue(FamilyDS.PROC_STATUS_VIEW)));
    }

    @Override
    public void setUiHandlers(FamilyUiHandlers uiHandlers) {
        this.uiHandlers = uiHandlers;
    }

}
