package org.siemac.metamac.statistical.operations.web.client.family.view;

import java.util.List;

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
import org.siemac.metamac.web.common.client.widgets.TitleLabel;
import org.siemac.metamac.web.common.client.widgets.form.GroupDynamicForm;
import org.siemac.metamac.web.common.client.widgets.form.fields.MultiLanguageTextItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.MultilanguageRichTextEditorItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.RequiredTextItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.ViewMultiLanguageTextItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.ViewTextItem;

import com.google.gwt.user.client.ui.Widget;
import com.gwtplatform.mvp.client.ViewWithUiHandlers;
import com.smartgwt.client.types.Autofit;
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

    public static final int                 OPERATION_LIST_MAX_RESULTS = 17;

    private FamilyUiHandlers                uiHandlers;

    private VLayout                         panel;

    private FamilyMainFormLayout            mainFormLayout;
    private GroupDynamicForm                form;
    private GroupDynamicForm                editionForm;

    private MultiLanguageTextItem           titleItem;
    private MultiLanguageTextItem           acronymItem;
    private MultilanguageRichTextEditorItem descriptionItem;

    private ToolStrip                       operationToolStrip;
    private ToolStripButton                 editToolStripButton;
    private BaseCustomListGrid              operationListGrid;

    private List<OperationBaseDto>          operationBaseDtos;

    // Add operations to family modal
    private AddOperationsToFamilyWindow     addOperationsToFamilyWindow;

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

        operationListGrid = new BaseCustomListGrid();
        operationListGrid.setAutoFitMaxRecords(20);
        operationListGrid.setAutoFitData(Autofit.VERTICAL);
        operationListGrid.setFields(ResourceListFieldUtils.getOperationFields());

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
        familyDto.setCode(editionForm.getValueAsString(FamilyDS.CODE));
        familyDto.setTitle(titleItem.getValue());
        familyDto.setAcronym(acronymItem.getValue());
        familyDto.setDescription(descriptionItem.getValue());
        return familyDto;
    }

    @Override
    public void setOperations(List<OperationBaseDto> operations, int firstResult, int totalResults) {
        addOperationsToFamilyWindow.setOperations(operations, firstResult, totalResults);
    }

    private void setFamilyViewMode(FamilyDto familyDto) {
        form.setValue(FamilyDS.CODE, familyDto.getCode());
        form.setValue(FamilyDS.TITLE, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(familyDto.getTitle()));
        form.setValue(FamilyDS.ACRONYM, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(familyDto.getAcronym()));
        form.setValue(FamilyDS.DESCRIPTION, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(familyDto.getDescription()));
        form.setValue(FamilyDS.PROC_STATUS, OperationsWeb.getCoreMessages().getString(OperationsWeb.getCoreMessages().procStatusEnum() + familyDto.getProcStatus().getName()));
        form.setValue(FamilyDS.CREATED_DATE, familyDto.getCreatedDate());
        form.setValue(FamilyDS.INTERNAL_INVENTORY_DATE, familyDto.getInternalInventoryDate());
        form.setValue(FamilyDS.INVENTORY_DATE, familyDto.getInventoryDate());
        form.setValue(FamilyDS.URN, familyDto.getUrn());
    }

    private void setFamilyEditionMode(FamilyDto familyDto) {
        editionForm.setValue(FamilyDS.CODE, familyDto.getCode());
        editionForm.setValue(FamilyDS.CODE_VIEW, familyDto.getCode());
        editionForm.setValue(FamilyDS.TITLE, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(familyDto.getTitle()));
        editionForm.setValue(FamilyDS.ACRONYM, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(familyDto.getAcronym()));
        editionForm.setValue(FamilyDS.DESCRIPTION, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(familyDto.getDescription()));
        editionForm.setValue(FamilyDS.PROC_STATUS, OperationsWeb.getCoreMessages().getString(OperationsWeb.getCoreMessages().procStatusEnum() + familyDto.getProcStatus().getName()));
        editionForm.setValue(FamilyDS.PROC_STATUS_VIEW, familyDto.getProcStatus().toString());
        editionForm.setValue(FamilyDS.CREATED_DATE, familyDto.getCreatedDate());
        editionForm.setValue(FamilyDS.INTERNAL_INVENTORY_DATE, familyDto.getInternalInventoryDate());
        editionForm.setValue(FamilyDS.INVENTORY_DATE, familyDto.getInventoryDate());
        editionForm.setValue(FamilyDS.URN, familyDto.getUrn());
        editionForm.markForRedraw();
    }

    @Override
    public HasRecordClickHandlers getSelectedOperation() {
        return operationListGrid;
    }

    @Override
    public boolean validate() {
        return editionForm.validate(false);
    }

    private void createViewForm() {
        // Family Form
        form = new GroupDynamicForm(OperationsWeb.getConstants().family());
        ViewTextItem code = new ViewTextItem(FamilyDS.CODE, OperationsWeb.getCoreMessages().family_code());
        ViewMultiLanguageTextItem title = new ViewMultiLanguageTextItem(FamilyDS.TITLE, OperationsWeb.getCoreMessages().family_title());
        ViewMultiLanguageTextItem acronym = new ViewMultiLanguageTextItem(FamilyDS.ACRONYM, OperationsWeb.getCoreMessages().family_acronym());
        ViewMultiLanguageTextItem description = new ViewMultiLanguageTextItem(FamilyDS.DESCRIPTION, OperationsWeb.getCoreMessages().family_description());
        ViewTextItem createdDate = new ViewTextItem(FamilyDS.CREATED_DATE, OperationsWeb.getConstants().familyCreatedDate());
        ViewTextItem procStatus = new ViewTextItem(FamilyDS.PROC_STATUS, OperationsWeb.getCoreMessages().family_proc_status());
        ViewTextItem internalInventoryDate = new ViewTextItem(FamilyDS.INTERNAL_INVENTORY_DATE, OperationsWeb.getCoreMessages().family_internal_inventory_date());
        ViewTextItem inventoryDate = new ViewTextItem(FamilyDS.INVENTORY_DATE, OperationsWeb.getCoreMessages().family_inventory_date());
        ViewTextItem urn = new ViewTextItem(FamilyDS.URN, OperationsWeb.getCoreMessages().family_urn());
        form.setFields(code, title, acronym, procStatus, description, createdDate, internalInventoryDate, inventoryDate, urn);
        // Add to main layout
        mainFormLayout.addViewCanvas(form);
    }

    private void createEditionForm() {
        // Family Form
        editionForm = new GroupDynamicForm(OperationsWeb.getConstants().family());

        // Code
        RequiredTextItem code = new RequiredTextItem(FamilyDS.CODE, OperationsWeb.getCoreMessages().family_code());
        code.setShowIfCondition(new FormItemIfFunction() {

            @Override
            public boolean execute(FormItem item, Object value, DynamicForm form) {
                return canFamilyCodeBeEdited(form);
            }
        });
        code.setValidators(CommonWebUtils.getSemanticIdentifierCustomValidator());

        ViewTextItem staticCode = new ViewTextItem(FamilyDS.CODE_VIEW, OperationsWeb.getCoreMessages().family_code());
        staticCode.setShowIfCondition(new FormItemIfFunction() {

            @Override
            public boolean execute(FormItem item, Object value, DynamicForm form) {
                return !canFamilyCodeBeEdited(form);
            }
        });

        titleItem = new MultiLanguageTextItem(FamilyDS.TITLE, OperationsWeb.getCoreMessages().family_title());
        titleItem.setRequired(true);
        acronymItem = new MultiLanguageTextItem(FamilyDS.ACRONYM, OperationsWeb.getCoreMessages().family_acronym());
        descriptionItem = new MultilanguageRichTextEditorItem(FamilyDS.DESCRIPTION, OperationsWeb.getCoreMessages().family_description());

        // ProcStatus
        ViewTextItem procStatus = new ViewTextItem(FamilyDS.PROC_STATUS, OperationsWeb.getCoreMessages().family_proc_status());
        ViewTextItem staticProcStatus = new ViewTextItem(FamilyDS.PROC_STATUS_VIEW, OperationsWeb.getCoreMessages().family_proc_status());
        staticProcStatus.setShowIfCondition(FormItemUtils.getFalseFormItemIfFunction());

        ViewTextItem createdDate = new ViewTextItem(FamilyDS.CREATED_DATE, OperationsWeb.getConstants().familyCreatedDate());
        ViewTextItem internalInventoryDate = new ViewTextItem(FamilyDS.INTERNAL_INVENTORY_DATE, OperationsWeb.getCoreMessages().family_internal_inventory_date());
        ViewTextItem inventoryDate = new ViewTextItem(FamilyDS.INVENTORY_DATE, OperationsWeb.getCoreMessages().family_inventory_date());
        ViewTextItem urn = new ViewTextItem(FamilyDS.URN, OperationsWeb.getCoreMessages().family_urn());
        editionForm.setFields(staticCode, code, titleItem, acronymItem, procStatus, staticProcStatus, descriptionItem, createdDate, internalInventoryDate, inventoryDate, urn);
        // Add to main layout
        mainFormLayout.addEditionCanvas(editionForm);
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
        form.setTranslationsShowed(translationsShowed);
        editionForm.setTranslationsShowed(translationsShowed);
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
