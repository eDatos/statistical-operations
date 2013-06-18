package org.siemac.metamac.statistical.operations.web.client.family.view;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;

import java.util.List;

import org.siemac.metamac.core.common.dto.InternationalStringDto;
import org.siemac.metamac.statistical.operations.core.dto.FamilyDto;
import org.siemac.metamac.statistical.operations.core.dto.OperationBaseDto;
import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.web.client.OperationsWeb;
import org.siemac.metamac.statistical.operations.web.client.enums.ToolStripButtonEnum;
import org.siemac.metamac.statistical.operations.web.client.family.presenter.FamilyPresenter;
import org.siemac.metamac.statistical.operations.web.client.family.view.handlers.FamilyUiHandlers;
import org.siemac.metamac.statistical.operations.web.client.model.ds.FamilyDS;
import org.siemac.metamac.statistical.operations.web.client.resources.GlobalResources;
import org.siemac.metamac.statistical.operations.web.client.utils.ClientSecurityUtils;
import org.siemac.metamac.statistical.operations.web.client.utils.RecordUtils;
import org.siemac.metamac.statistical.operations.web.client.utils.ResourceListFieldUtils;
import org.siemac.metamac.statistical.operations.web.client.widgets.AddOperationsToFamilyWindow;
import org.siemac.metamac.statistical.operations.web.client.widgets.FamilyMainFormLayout;
import org.siemac.metamac.web.common.client.utils.CommonWebUtils;
import org.siemac.metamac.web.common.client.utils.FormItemUtils;
import org.siemac.metamac.web.common.client.utils.InternationalStringUtils;
import org.siemac.metamac.web.common.client.widgets.BaseCustomListGrid;
import org.siemac.metamac.web.common.client.widgets.CustomListGridSectionStack;
import org.siemac.metamac.web.common.client.widgets.form.GroupDynamicForm;
import org.siemac.metamac.web.common.client.widgets.form.fields.MultiLanguageTextItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.MultilanguageRichTextEditorItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.RequiredTextItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.ViewMultiLanguageTextItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.ViewTextItem;

import com.google.gwt.user.client.ui.Widget;
import com.gwtplatform.mvp.client.ViewWithUiHandlers;
import com.smartgwt.client.types.Autofit;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.Visibility;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.HasClickHandlers;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.FormItemIfFunction;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.grid.events.HasRecordClickHandlers;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

public class FamilyViewImpl extends ViewWithUiHandlers<FamilyUiHandlers> implements FamilyPresenter.FamilyView {

    public static final int             OPERATION_LIST_MAX_RESULTS = 6;

    private FamilyUiHandlers            uiHandlers;

    private VLayout                     panel;

    private FamilyMainFormLayout        mainFormLayout;

    // View forms
    private GroupDynamicForm            identifiersForm;
    private GroupDynamicForm            contentDescriptorsForm;
    private GroupDynamicForm            productionDescriptorsForm;
    private GroupDynamicForm            diffusionDescriptorsForm;

    // EditionForms
    private GroupDynamicForm            identifiersEditionForm;
    private GroupDynamicForm            contentDescriptorsEditionForm;
    private GroupDynamicForm            productionDescriptorsEditionForm;
    private GroupDynamicForm            diffusionDescriptorsEditionForm;

    private ToolStrip                   operationToolStrip;
    private ToolStripButton             editToolStripButton;
    private BaseCustomListGrid          operationListGrid;

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

        operationListGrid = new BaseCustomListGrid();
        operationListGrid.setAutoFitMaxRecords(20);
        operationListGrid.setAutoFitData(Autofit.VERTICAL);
        operationListGrid.setFields(ResourceListFieldUtils.getOperationFields());

        CustomListGridSectionStack operationsSectionStack = new CustomListGridSectionStack(operationListGrid, getConstants().operations(), "sectionStackStyle");
        operationsSectionStack.setMargin(15);
        operationsSectionStack.getDefaultSection().setItems(operationToolStrip, operationListGrid);
        operationsSectionStack.getDefaultSection().setExpanded(true);

        VLayout subPanel = new VLayout();
        subPanel.setOverflow(Overflow.SCROLL);
        subPanel.addMember(mainFormLayout);
        subPanel.addMember(operationsSectionStack);

        panel.addMember(subPanel);
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
                Canvas[] canvas = ((ToolStrip) content).getMembers();
                for (int i = 0; i < canvas.length; i++) {
                    if (canvas[i] instanceof ToolStripButton) {
                        if (ToolStripButtonEnum.FAMILIES.getValue().equals(((ToolStripButton) canvas[i]).getID())) {
                            ((ToolStripButton) canvas[i]).select();
                        }
                    }
                }
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
        familyDto.setCode(identifiersEditionForm.getValueAsString(FamilyDS.CODE));
        familyDto.setTitle((InternationalStringDto) identifiersEditionForm.getValue(FamilyDS.TITLE));
        familyDto.setAcronym((InternationalStringDto) identifiersEditionForm.getValue(FamilyDS.ACRONYM));
        familyDto.setDescription((InternationalStringDto) contentDescriptorsEditionForm.getValue(FamilyDS.DESCRIPTION));
        return familyDto;
    }

    @Override
    public void setOperations(List<OperationBaseDto> operations, int firstResult, int totalResults) {
        addOperationsToFamilyWindow.setOperations(operations, firstResult, totalResults);
    }

    private void setFamilyViewMode(FamilyDto familyDto) {

        // IDENTIFIERS FORM

        identifiersForm.setValue(FamilyDS.CODE, familyDto.getCode());
        identifiersForm.setValue(FamilyDS.TITLE, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(familyDto.getTitle()));
        identifiersForm.setValue(FamilyDS.ACRONYM, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(familyDto.getAcronym()));
        identifiersForm.setValue(FamilyDS.URN, familyDto.getUrn());

        // CONTENT DESCRIPTORS FORM

        contentDescriptorsForm.setValue(FamilyDS.DESCRIPTION, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(familyDto.getDescription()));

        // PRODUCTION DESCRIPTORS FORM

        productionDescriptorsForm.setValue(FamilyDS.PROC_STATUS, OperationsWeb.getCoreMessages().getString(OperationsWeb.getCoreMessages().procStatusEnum() + familyDto.getProcStatus().getName()));
        productionDescriptorsForm.setValue(FamilyDS.CREATED_DATE, familyDto.getCreatedDate());
        productionDescriptorsForm.setValue(FamilyDS.INTERNAL_INVENTORY_DATE, familyDto.getInternalInventoryDate());

        // DIFFUSION DESCRIPTORS FORM

        diffusionDescriptorsForm.setValue(FamilyDS.INVENTORY_DATE, familyDto.getInventoryDate());

    }

    private void setFamilyEditionMode(FamilyDto familyDto) {

        // IDENTIFIERS FORM

        identifiersEditionForm.setValue(FamilyDS.CODE, familyDto.getCode());
        identifiersEditionForm.setValue(FamilyDS.CODE_VIEW, familyDto.getCode());
        identifiersEditionForm.setValue(FamilyDS.TITLE, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(familyDto.getTitle()));
        identifiersEditionForm.setValue(FamilyDS.ACRONYM, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(familyDto.getAcronym()));
        identifiersEditionForm.setValue(FamilyDS.URN, familyDto.getUrn());
        identifiersEditionForm.markForRedraw();

        // CONTENT DESCRIPTORS FORM

        contentDescriptorsEditionForm.setValue(FamilyDS.DESCRIPTION, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(familyDto.getDescription()));

        // PRODUCTION DESCRIPTORS FORM

        productionDescriptorsEditionForm.setValue(FamilyDS.PROC_STATUS,
                OperationsWeb.getCoreMessages().getString(OperationsWeb.getCoreMessages().procStatusEnum() + familyDto.getProcStatus().getName()));
        productionDescriptorsEditionForm.setValue(FamilyDS.PROC_STATUS_VIEW, familyDto.getProcStatus().toString());
        productionDescriptorsEditionForm.setValue(FamilyDS.CREATED_DATE, familyDto.getCreatedDate());
        productionDescriptorsEditionForm.setValue(FamilyDS.INTERNAL_INVENTORY_DATE, familyDto.getInternalInventoryDate());

        // DIFFUSION DESCRIPTORS FORM

        diffusionDescriptorsEditionForm.setValue(FamilyDS.INVENTORY_DATE, familyDto.getInventoryDate());

    }

    @Override
    public HasRecordClickHandlers getSelectedOperation() {
        return operationListGrid;
    }

    @Override
    public boolean validate() {
        return identifiersEditionForm.validate(false);
    }

    private void createViewForm() {

        // IDENTIFIERS FORM

        identifiersForm = new GroupDynamicForm(getConstants().formIdentifiers());
        ViewTextItem code = new ViewTextItem(FamilyDS.CODE, getConstants().familyCode());
        ViewMultiLanguageTextItem title = new ViewMultiLanguageTextItem(FamilyDS.TITLE, getConstants().familyTitle());
        ViewMultiLanguageTextItem acronym = new ViewMultiLanguageTextItem(FamilyDS.ACRONYM, getConstants().familyAcronym());
        ViewTextItem urn = new ViewTextItem(FamilyDS.URN, getConstants().familyUrn());
        identifiersForm.setFields(code, title, acronym, urn);

        // CONTENT DESCRIPTORS FORM

        contentDescriptorsForm = new GroupDynamicForm(getConstants().formContentDescriptors());
        ViewMultiLanguageTextItem description = new ViewMultiLanguageTextItem(FamilyDS.DESCRIPTION, getConstants().familyDescription());
        contentDescriptorsForm.setFields(description);

        // PRODUCTION DESCRIPTORS FORM

        productionDescriptorsForm = new GroupDynamicForm(getConstants().formProductionDescriptors());
        ViewTextItem createdDate = new ViewTextItem(FamilyDS.CREATED_DATE, getConstants().familyCreatedDate());
        ViewTextItem internalInventoryDate = new ViewTextItem(FamilyDS.INTERNAL_INVENTORY_DATE, getConstants().familyInternalInventoryDate());
        ViewTextItem procStatus = new ViewTextItem(FamilyDS.PROC_STATUS, getConstants().familyProcStatus());
        productionDescriptorsForm.setFields(createdDate, internalInventoryDate, procStatus);

        // DIFFUSION DESCRIPTORS FORM

        diffusionDescriptorsForm = new GroupDynamicForm(getConstants().formDiffusionDescriptors());
        ViewTextItem inventoryDate = new ViewTextItem(FamilyDS.INVENTORY_DATE, getConstants().familyInventoryDate());
        diffusionDescriptorsForm.setFields(inventoryDate);

        // Add to main layout
        mainFormLayout.addViewCanvas(identifiersForm);
        mainFormLayout.addViewCanvas(contentDescriptorsForm);
        mainFormLayout.addViewCanvas(productionDescriptorsForm);
        mainFormLayout.addViewCanvas(diffusionDescriptorsForm);
    }

    private void createEditionForm() {

        // IDENTIFIERS FORM

        identifiersEditionForm = new GroupDynamicForm(getConstants().family());

        // Code

        RequiredTextItem code = new RequiredTextItem(FamilyDS.CODE, getConstants().familyCode());
        code.setShowIfCondition(new FormItemIfFunction() {

            @Override
            public boolean execute(FormItem item, Object value, DynamicForm form) {
                return canFamilyCodeBeEdited(form);
            }
        });
        code.setValidators(CommonWebUtils.getSemanticIdentifierCustomValidator());

        ViewTextItem staticCode = new ViewTextItem(FamilyDS.CODE_VIEW, getConstants().familyCode());
        staticCode.setShowIfCondition(new FormItemIfFunction() {

            @Override
            public boolean execute(FormItem item, Object value, DynamicForm form) {
                return !canFamilyCodeBeEdited(form);
            }
        });

        MultiLanguageTextItem titleItem = new MultiLanguageTextItem(FamilyDS.TITLE, getConstants().familyTitle());
        titleItem.setRequired(true);
        MultiLanguageTextItem acronymItem = new MultiLanguageTextItem(FamilyDS.ACRONYM, getConstants().familyAcronym());
        ViewTextItem urn = new ViewTextItem(FamilyDS.URN, getConstants().familyUrn());
        identifiersEditionForm.setFields(staticCode, code, titleItem, acronymItem, urn);

        // CONTENT DESCRIPTORS FORM

        contentDescriptorsEditionForm = new GroupDynamicForm(getConstants().formContentDescriptors());
        MultilanguageRichTextEditorItem descriptionItem = new MultilanguageRichTextEditorItem(FamilyDS.DESCRIPTION, OperationsWeb.getConstants().familyDescription());
        contentDescriptorsEditionForm.setFields(descriptionItem);

        // PRODUCTION DESCRIPTORS FORM

        productionDescriptorsEditionForm = new GroupDynamicForm(getConstants().formProductionDescriptors());
        ViewTextItem createdDate = new ViewTextItem(FamilyDS.CREATED_DATE, OperationsWeb.getConstants().familyCreatedDate());
        ViewTextItem internalInventoryDate = new ViewTextItem(FamilyDS.INTERNAL_INVENTORY_DATE, OperationsWeb.getConstants().familyInternalInventoryDate());
        ViewTextItem procStatus = new ViewTextItem(FamilyDS.PROC_STATUS, OperationsWeb.getConstants().familyProcStatus());
        ViewTextItem staticProcStatus = new ViewTextItem(FamilyDS.PROC_STATUS_VIEW, OperationsWeb.getConstants().familyProcStatus());
        staticProcStatus.setShowIfCondition(FormItemUtils.getFalseFormItemIfFunction());
        productionDescriptorsEditionForm.setFields(createdDate, internalInventoryDate, procStatus, staticProcStatus);

        // DIFFUSION DESCRIPTORS FORM

        diffusionDescriptorsEditionForm = new GroupDynamicForm(getConstants().formDiffusionDescriptors());
        ViewTextItem inventoryDate = new ViewTextItem(FamilyDS.INVENTORY_DATE, getConstants().familyInventoryDate());
        diffusionDescriptorsEditionForm.setFields(inventoryDate);

        // Add to main layout
        mainFormLayout.addEditionCanvas(identifiersEditionForm);
        mainFormLayout.addEditionCanvas(contentDescriptorsEditionForm);
        mainFormLayout.addEditionCanvas(productionDescriptorsEditionForm);
        mainFormLayout.addEditionCanvas(diffusionDescriptorsEditionForm);
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
        setFamilyViewMode(familyDto);
        setFamilyEditionMode(familyDto);
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
        identifiersForm.setTranslationsShowed(translationsShowed);
        identifiersEditionForm.setTranslationsShowed(translationsShowed);
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
