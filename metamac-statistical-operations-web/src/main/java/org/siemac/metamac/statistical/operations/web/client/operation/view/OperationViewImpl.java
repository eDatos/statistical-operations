package org.siemac.metamac.statistical.operations.web.client.operation.view;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;
import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getCoreMessages;

import java.util.ArrayList;
import java.util.List;

import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.core.common.dto.InternationalStringDto;
import org.siemac.metamac.core.common.util.shared.StringUtils;
import org.siemac.metamac.statistical.operations.core.dto.FamilyBaseDto;
import org.siemac.metamac.statistical.operations.core.dto.InstanceBaseDto;
import org.siemac.metamac.statistical.operations.core.dto.InstanceDto;
import org.siemac.metamac.statistical.operations.core.dto.OfficialityTypeDto;
import org.siemac.metamac.statistical.operations.core.dto.OperationDto;
import org.siemac.metamac.statistical.operations.core.dto.SurveyTypeDto;
import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.core.enume.domain.StatusEnum;
import org.siemac.metamac.statistical.operations.web.client.enums.ToolStripButtonEnum;
import org.siemac.metamac.statistical.operations.web.client.model.InstanceRecord;
import org.siemac.metamac.statistical.operations.web.client.model.ds.InstanceDS;
import org.siemac.metamac.statistical.operations.web.client.model.ds.OperationDS;
import org.siemac.metamac.statistical.operations.web.client.operation.presenter.OperationPresenter;
import org.siemac.metamac.statistical.operations.web.client.operation.view.handlers.OperationUiHandlers;
import org.siemac.metamac.statistical.operations.web.client.resources.GlobalResources;
import org.siemac.metamac.statistical.operations.web.client.utils.ClientSecurityUtils;
import org.siemac.metamac.statistical.operations.web.client.utils.CommonUtils;
import org.siemac.metamac.statistical.operations.web.client.utils.OperationsListUtils;
import org.siemac.metamac.statistical.operations.web.client.utils.PlaceRequestUtils;
import org.siemac.metamac.statistical.operations.web.client.utils.RecordUtils;
import org.siemac.metamac.statistical.operations.web.client.utils.ResourceListFieldUtils;
import org.siemac.metamac.statistical.operations.web.client.widgets.AddFamiliesToOperationWindow;
import org.siemac.metamac.statistical.operations.web.client.widgets.InstancesOrderFormLayout;
import org.siemac.metamac.statistical.operations.web.client.widgets.ListGridToolStrip;
import org.siemac.metamac.statistical.operations.web.client.widgets.ModalWindow;
import org.siemac.metamac.statistical.operations.web.client.widgets.NewInstanceForm;
import org.siemac.metamac.statistical.operations.web.client.widgets.OperationMainFormLayout;
import org.siemac.metamac.statistical.operations.web.client.widgets.external.ExternalItemListItem;
import org.siemac.metamac.statistical.operations.web.client.widgets.external.MultipleExternalResourceAction;
import org.siemac.metamac.statistical.operations.web.client.widgets.external.SearchCategoryItem;
import org.siemac.metamac.statistical.operations.web.client.widgets.external.SearchItemItem;
import org.siemac.metamac.statistical.operations.web.client.widgets.external.SearchMultipleCategoriesItem;
import org.siemac.metamac.statistical.operations.web.client.widgets.external.SearchMultipleItemsItem;
import org.siemac.metamac.statistical.operations.web.client.widgets.external.SearchMultipleOrganisationUntisItem;
import org.siemac.metamac.web.common.client.MetamacWebCommon;
import org.siemac.metamac.web.common.client.utils.CommonWebUtils;
import org.siemac.metamac.web.common.client.utils.ExternalItemUtils;
import org.siemac.metamac.web.common.client.utils.FormItemUtils;
import org.siemac.metamac.web.common.client.utils.InternationalStringUtils;
import org.siemac.metamac.web.common.client.view.handlers.BaseUiHandlers;
import org.siemac.metamac.web.common.client.widgets.BaseCustomListGrid;
import org.siemac.metamac.web.common.client.widgets.CustomListGrid;
import org.siemac.metamac.web.common.client.widgets.TitleLabel;
import org.siemac.metamac.web.common.client.widgets.form.GroupDynamicForm;
import org.siemac.metamac.web.common.client.widgets.form.fields.CustomCheckboxItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.CustomLinkItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.CustomSelectItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.CustomTextItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.ExternalItemLinkItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.ExternalMultipleSelectItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.MultiLanguageTextItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.MultilanguageRichTextEditorItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.RequiredTextItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.SearchExternalItemLinkItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.ViewMultiLanguageTextItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.ViewTextItem;
import org.siemac.metamac.web.common.client.widgets.handlers.CustomLinkItemNavigationClickHandler;
import org.siemac.metamac.web.common.shared.domain.ExternalItemsResult;

import com.google.gwt.user.client.ui.Widget;
import com.gwtplatform.mvp.client.ViewWithUiHandlers;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.SortDirection;
import com.smartgwt.client.types.Visibility;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.HasClickHandlers;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.FormItemIfFunction;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.HasRecordClickHandlers;
import com.smartgwt.client.widgets.grid.events.SelectionChangedHandler;
import com.smartgwt.client.widgets.grid.events.SelectionEvent;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

public class OperationViewImpl extends ViewWithUiHandlers<OperationUiHandlers> implements OperationPresenter.OperationView {

    public static final int              FAMILY_LIST_MAX_RESULTS = 17;

    private VLayout                      panel;

    private OperationMainFormLayout      mainFormLayout;

    private OperationDto                 operationDto;

    // IDENTIFIERS
    private GroupDynamicForm             identifiersForm;
    private GroupDynamicForm             identifiersEditionForm;

    // CONTENT CLASSIFIERS
    private GroupDynamicForm             contentClassifiersForm;
    private GroupDynamicForm             contentClassifiersEditionForm;

    // CONTENT DESCRIPTORS
    private GroupDynamicForm             contentViewForm;
    private GroupDynamicForm             contentEditionForm;

    // CLASS DESCRIPTORS
    private GroupDynamicForm             classForm;
    private GroupDynamicForm             classEditionForm;
    private CustomSelectItem             surveyType;
    private CustomSelectItem             officialityType;
    private CustomCheckboxItem           indSystem;

    // PRODUCTION DESCRIPTORS
    private GroupDynamicForm             productionDescriptorsForm;
    private GroupDynamicForm             productionDescriptorsEditionForm;
    private ExternalMultipleSelectItem   regionalResponsibleItem;
    private ExternalMultipleSelectItem   regionalContributorItem;
    private CustomCheckboxItem           currentlyActiveItem;
    private CustomSelectItem             statusItem;

    // DIFUSSION AND PUBLICATION
    private GroupDynamicForm             diffusionForm;
    private GroupDynamicForm             diffusionEditionForm;
    private ExternalMultipleSelectItem   publisherItem;
    private CustomCheckboxItem           releaseCalendar;
    private CustomTextItem               releaseCalendarAccess;
    private CustomSelectItem             updateFrequencyItem;
    private CustomSelectItem             commonMetadataItem;

    // LEGAL ACTS
    private GroupDynamicForm             legalActsForm;
    private GroupDynamicForm             legalActsEditionForm;

    // ANNOTATIONS
    private GroupDynamicForm             annotationsViewForm;
    private GroupDynamicForm             annotationsEditionForm;

    private ListGridToolStrip            instanceListGridToolStrip;
    private CustomListGrid               instanceListGrid;
    private InstancesOrderFormLayout     instancesOrderFormLayout;
    // Instance modal window
    private ModalWindow                  newInstanceWindow;
    private NewInstanceForm              newInstanceForm;

    private ToolStrip                    familiesToolStrip;
    private ToolStripButton              editFamiliesToolStripButton;
    private BaseCustomListGrid           familyListGrid;
    // Families modal window
    private AddFamiliesToOperationWindow addFamiliesToOperationWindow;

    private List<FamilyBaseDto>          familyBaseDtos;

    private List<SurveyTypeDto>          surveyTypeDtos;
    private List<OfficialityTypeDto>     officialityTypeDtos;

    private List<ExternalItemDto>        regionalResponsibles;
    private List<ExternalItemDto>        regionalContributors;
    private List<ExternalItemDto>        publishers;
    private List<ExternalItemDto>        commonMetadataList;
    private List<ExternalItemDto>        updateFrequencyCodes;

    public OperationViewImpl() {
        super();
        panel = new VLayout();

        // OPERATION

        mainFormLayout = new OperationMainFormLayout();
        mainFormLayout.getTranslateToolStripButton().addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                setTranslationsShowed(mainFormLayout.getTranslateToolStripButton().isSelected());
            }
        });
        createViewForm();
        createEditionForm();

        // INSTANCES

        newInstanceForm = new NewInstanceForm();

        newInstanceWindow = new ModalWindow();
        newInstanceWindow.setTitle(getConstants().actionNewInstance());
        newInstanceWindow.setAutoSize(true);
        newInstanceWindow.addItem(newInstanceForm);

        instanceListGridToolStrip = new ListGridToolStrip(getConstants().instanceDeleteConfirmation());
        instanceListGridToolStrip.getNewButton().addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                // Clear new instance form
                newInstanceForm.clearValues();

                newInstanceWindow.show();
            }
        });

        TitleLabel instancesTitleLabel = new TitleLabel(getConstants().instances());
        instancesTitleLabel.setStyleName("sectionTitleLeftMargin");

        // Instances list
        instanceListGrid = new CustomListGrid();
        instanceListGrid.setHeight(150);
        instanceListGrid.setFields(ResourceListFieldUtils.getInstanceFields());
        instanceListGrid.addSelectionChangedHandler(new SelectionChangedHandler() {

            @Override
            public void onSelectionChanged(SelectionEvent event) {
                if (instanceListGrid.getSelectedRecords() != null && instanceListGrid.getSelectedRecords().length == 1) {
                    InstanceRecord record = (InstanceRecord) instanceListGrid.getSelectedRecord();
                    selectInstance(record.getId());
                } else {
                    // No record selected
                    deselectInstance();
                    if (instanceListGrid.getSelectedRecords().length > 1) {
                        // Delete more than one Instance with one click
                        showInstanceListGridDeleteButton();
                    }
                }
            }
        });

        VLayout instancesListGridLayout = new VLayout();
        instancesListGridLayout.setMargin(15);
        instancesListGridLayout.addMember(instanceListGridToolStrip);
        instancesListGridLayout.addMember(instanceListGrid);

        // Instances order
        instancesOrderFormLayout = new InstancesOrderFormLayout();
        instancesOrderFormLayout.getSave().addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                getUiHandlers().updateInstancesOrder(instancesOrderFormLayout.getInstancesOrder());
            }
        });

        // FAMILIES

        familiesToolStrip = new ToolStrip();
        familiesToolStrip.setWidth100();
        editFamiliesToolStripButton = new ToolStripButton(getConstants().actionEdit(), GlobalResources.RESOURCE.editListGrid().getURL());
        editFamiliesToolStripButton.addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                // Load operation families
                getUiHandlers().retrievePaginatedFamilies(0, FAMILY_LIST_MAX_RESULTS, null);

                addFamiliesToOperationWindow = new AddFamiliesToOperationWindow(getUiHandlers());
                addFamiliesToOperationWindow.setSelectedFamilies(familyBaseDtos);
            }
        });
        familiesToolStrip.addButton(editFamiliesToolStripButton);

        TitleLabel familiesTitleLabel = new TitleLabel(getConstants().families());
        familiesTitleLabel.setStyleName("sectionTitleLeftMargin");

        familyListGrid = new BaseCustomListGrid();
        familyListGrid.setHeight(150);
        familyListGrid.setFields(ResourceListFieldUtils.getFamilyFields());

        VLayout familiesListGridLayout = new VLayout();
        familiesListGridLayout.setMargin(15);
        familiesListGridLayout.addMember(familiesToolStrip);
        familiesListGridLayout.addMember(familyListGrid);

        VLayout subPanel = new VLayout();
        subPanel.setHeight100();
        subPanel.setOverflow(Overflow.SCROLL);
        subPanel.addMember(mainFormLayout);

        subPanel.addMember(instancesTitleLabel);
        subPanel.addMember(instancesListGridLayout);
        subPanel.addMember(instancesOrderFormLayout);

        subPanel.addMember(familiesTitleLabel);
        subPanel.addMember(familiesListGridLayout);

        panel.addMember(subPanel);
    }

    @Override
    public Widget asWidget() {
        return panel;
    }

    @Override
    public void setUiHandlers(OperationUiHandlers uiHandlers) {
        super.setUiHandlers(uiHandlers);

        // Set uiHandlers in formItems

        ((SearchItemItem) contentClassifiersEditionForm.getItem(OperationDS.SUBJECT_AREA)).setUiHandlers(uiHandlers);
        ((SearchMultipleItemsItem) contentClassifiersEditionForm.getItem(OperationDS.SECONDARY_SUBJECT_AREAS)).setUiHandlers(uiHandlers);

        ((SearchMultipleItemsItem) productionDescriptorsEditionForm.getItem(OperationDS.PRODUCER)).setUiHandlers(uiHandlers);
    }

    /*
     * GWTP will call setInSlot when a child presenter asks to be added under this view
     */
    @Override
    public void setInSlot(Object slot, Widget content) {
        if (slot == OperationPresenter.TYPE_SetContextAreaContentToolBar) {
            if (content != null) {
                Canvas[] canvas = ((ToolStrip) content).getMembers();
                for (int i = 0; i < canvas.length; i++) {
                    if (canvas[i] instanceof ToolStripButton) {
                        if (ToolStripButtonEnum.OPERATIONS.getValue().equals(((ToolStripButton) canvas[i]).getID())) {
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
    public HasRecordClickHandlers getSelectedFamily() {
        return familyListGrid;
    }

    @Override
    public HasRecordClickHandlers getSelectedInstance() {
        return instanceListGrid;
    }

    @Override
    public void setOperation(OperationDto operationDto, List<InstanceBaseDto> instanceBaseDtos, List<FamilyBaseDto> familyBaseDtos) {
        this.operationDto = operationDto;

        // Security
        mainFormLayout.setCanEdit(ClientSecurityUtils.canUpdateOperation(operationDto.getCode()));
        mainFormLayout.setOperationCode(operationDto.getCode());
        instancesOrderFormLayout.setCanEdit(ClientSecurityUtils.canUpdateInstancesOrder(operationDto.getCode()));
        instanceListGridToolStrip.getNewButton().setVisibility(ClientSecurityUtils.canCreateInstance(operationDto.getCode()) ? Visibility.VISIBLE : Visibility.HIDDEN);
        editFamiliesToolStripButton.setVisibility(ClientSecurityUtils.canAddFamilyToOperation(operationDto.getCode()) ? Visibility.VISIBLE : Visibility.HIDDEN);

        // Load common metadata configurations
        getUiHandlers().retrieveCommonMetadataConfigurations();

        // Operation
        setOperation(operationDto);

        // Set Instances
        setInstances(instanceBaseDtos);

        // Set Families
        setOperationFamilies(familyBaseDtos);
    }

    @Override
    public OperationDto getOperation(OperationDto operationDto) {

        // IDENTIFIERS

        operationDto.setCode(identifiersEditionForm.getValueAsString(OperationDS.CODE));
        operationDto.setTitle((InternationalStringDto) identifiersEditionForm.getValue(OperationDS.TITLE));
        operationDto.setAcronym((InternationalStringDto) identifiersEditionForm.getValue(OperationDS.ACRONYM));

        // CONTENT CLASSIFIERS

        operationDto.setSubjectArea(((ExternalItemLinkItem) contentClassifiersEditionForm.getItem(OperationDS.SUBJECT_AREA)).getExternalItemDto());

        List<ExternalItemDto> secondarySubjectAreas = ((ExternalItemListItem) contentClassifiersEditionForm.getItem(OperationDS.SECONDARY_SUBJECT_AREAS)).getExternalItemDtos();
        operationDto.getSecondarySubjectAreas().clear();
        operationDto.getSecondarySubjectAreas().addAll(secondarySubjectAreas);

        // CONTENT DESCRIPTORS

        operationDto.setDescription((InternationalStringDto) contentEditionForm.getValue(OperationDS.DESCRIPTION));
        operationDto.setObjective((InternationalStringDto) contentEditionForm.getValue(OperationDS.OBJECTIVE));

        // CLASS DESCRIPTORS

        operationDto.setSurveyType(OperationsListUtils.getSurveyTypeDto(surveyType.getValueAsString(), surveyTypeDtos));
        operationDto.setOfficialityType(OperationsListUtils.getOfficialityTypeDto(officialityType.getValueAsString(), officialityTypeDtos));
        operationDto.setIndicatorSystem(indSystem.getValueAsBoolean() == null ? false : indSystem.getValueAsBoolean());

        // PRODUCTION DESCRIPTORS

        List<ExternalItemDto> producers = ((ExternalItemListItem) productionDescriptorsEditionForm.getItem(OperationDS.PRODUCER)).getExternalItemDtos();
        operationDto.getProducer().clear();
        operationDto.getProducer().addAll(producers);

        operationDto.getRegionalResponsible().clear();
        operationDto.getRegionalResponsible().addAll(regionalResponsibleItem.getSelectedExternalItems(regionalResponsibles));
        operationDto.getRegionalContributor().clear();
        operationDto.getRegionalContributor().addAll(regionalContributorItem.getSelectedExternalItems(regionalContributors));
        operationDto.setCurrentlyActive(currentlyActiveItem.getValueAsBoolean());
        operationDto.setStatus(statusItem.getValueAsString() != null ? StatusEnum.valueOf(statusItem.getValueAsString()) : null);

        // DIFFUSION AND PUBLICATION

        operationDto.getPublisher().clear();
        operationDto.getPublisher().addAll(publisherItem.getSelectedExternalItems(publishers));
        operationDto.setRelPolUsAc((InternationalStringDto) diffusionEditionForm.getValue(OperationDS.RE_POL_US_AC));
        operationDto.setReleaseCalendar(releaseCalendar.getValueAsBoolean());
        operationDto.setReleaseCalendarAccess(releaseCalendarAccess.getValueAsString());
        operationDto.getUpdateFrequency().clear();
        operationDto.getUpdateFrequency().addAll(ExternalItemUtils.getExternalItemDtoListFromUrns(updateFrequencyCodes, updateFrequencyItem.getValues()));
        operationDto.setRevPolicy((InternationalStringDto) diffusionEditionForm.getValue(OperationDS.REV_POLICY));
        operationDto.setRevPractice((InternationalStringDto) diffusionEditionForm.getValue(OperationDS.REV_PRACTICE));
        operationDto.setCommonMetadata(ExternalItemUtils.getExternalItemDtoFromUrn(commonMetadataList, commonMetadataItem.getValueAsString()));

        // LEGAL ACTS
        operationDto.setSpecificLegalActs((InternationalStringDto) legalActsEditionForm.getValue(OperationDS.SPECIFIC_LEGAL_ACTS));
        operationDto.setSpecificDataSharing((InternationalStringDto) legalActsEditionForm.getValue(OperationDS.SPECIFIC_DATA_SHARING));

        // ANNOTATIONS

        operationDto.setComment((InternationalStringDto) annotationsEditionForm.getValue(OperationDS.COMMENTS));
        operationDto.setNotes((InternationalStringDto) annotationsEditionForm.getValue(OperationDS.NOTES));
        return operationDto;
    }

    @Override
    public HasClickHandlers getSave() {
        return mainFormLayout.getSave();
    }

    @Override
    public void onOperationSaved(OperationDto operationDto) {
        setOperation(operationDto);
    }

    @Override
    public boolean validate() {
        return identifiersEditionForm.validate(false) && productionDescriptorsEditionForm.validate(false) && contentEditionForm.validate(false) && contentClassifiersEditionForm.validate(false)
                && diffusionEditionForm.validate(false);
    }

    @Override
    public com.smartgwt.client.widgets.form.fields.events.HasClickHandlers getSaveNewInstance() {
        return newInstanceForm.getSave();
    }

    @Override
    public InstanceDto getNewInstace() {
        return newInstanceForm.getInstance();
    }

    @Override
    public void onInstanceSaved(InstanceDto instanceDto) {
        InstanceRecord record = RecordUtils.getInstanceRecord(instanceDto);
        instanceListGrid.addData(record);
        instanceListGrid.sort(InstanceDS.ORDER, SortDirection.DESCENDING);

        instancesOrderFormLayout.addInstance(instanceDto);
    }

    @Override
    public boolean validateNewInstance() {
        return newInstanceForm.validate();
    }

    @Override
    public void closeInstanceWindow() {
        newInstanceWindow.hide();
    }

    @Override
    public HasClickHandlers getDeleteInstance() {
        return instanceListGridToolStrip.getDeleteConfirmationWindow().getYesButton();
    }

    @Override
    public List<Long> getSelectedInstances() {
        List<Long> selectedInstances = new ArrayList<Long>();
        if (instanceListGrid.getSelectedRecords() != null) {
            ListGridRecord[] records = instanceListGrid.getSelectedRecords();
            for (int i = 0; i < records.length; i++) {
                InstanceRecord record = (InstanceRecord) records[i];
                selectedInstances.add(record.getId());
            }
        }
        return selectedInstances;
    }

    @Override
    public void setInstances(List<InstanceBaseDto> instanceBaseDtos) {
        // Instances list
        instanceListGrid.removeAllData();
        if (instanceBaseDtos != null) {
            for (InstanceBaseDto instanceBaseDto : instanceBaseDtos) {
                instanceListGrid.addData(RecordUtils.getInstanceRecord(instanceBaseDto));
            }
        }
        deselectInstance();

        // Instances order
        instancesOrderFormLayout.setInstances(instanceBaseDtos);
        if (instanceBaseDtos == null || instanceBaseDtos.isEmpty()) {
            instancesOrderFormLayout.hide();
        } else {
            instancesOrderFormLayout.show();
        }
    }

    public void setOperationFamilies(List<FamilyBaseDto> familyBaseDtos) {
        this.familyBaseDtos = familyBaseDtos;
        // Set families in listGrid
        familyListGrid.selectAllRecords();
        familyListGrid.removeSelectedData();
        familyListGrid.deselectAllRecords();
        if (familyBaseDtos != null) {
            for (FamilyBaseDto familyBaseDto : familyBaseDtos) {
                familyListGrid.addData(RecordUtils.getFamilyRecord(familyBaseDto));
            }
        }
    }

    private void setOperation(OperationDto operationDto) {
        mainFormLayout.setViewMode();
        mainFormLayout.updatePublishSection(operationDto.getProcStatus());
        // Set Family
        mainFormLayout.setTitleLabelContents(InternationalStringUtils.getLocalisedString(operationDto.getTitle()));
        // Form
        setOperationViewMode(operationDto);
        setOperationEditionMode(operationDto);
    }

    private void createViewForm() {
        // Identifiers
        identifiersForm = new GroupDynamicForm(getConstants().operationIdentifiers());
        ViewTextItem identifier = new ViewTextItem(OperationDS.CODE, getCoreMessages().operation_code());
        ViewMultiLanguageTextItem title = new ViewMultiLanguageTextItem(OperationDS.TITLE, getCoreMessages().operation_title());
        ViewMultiLanguageTextItem acronym = new ViewMultiLanguageTextItem(OperationDS.ACRONYM, getCoreMessages().operation_acronym());
        ViewTextItem urn = new ViewTextItem(OperationDS.URN, getCoreMessages().operation_urn());
        identifiersForm.setFields(identifier, title, acronym, urn);

        // Content Classifiers
        contentClassifiersForm = new GroupDynamicForm(getConstants().operationContentClassifiers());
        ExternalItemLinkItem subjectArea = new ExternalItemLinkItem(OperationDS.SUBJECT_AREA, getCoreMessages().operation_subject_area());
        ExternalItemListItem secondarySubject = new ExternalItemListItem(OperationDS.SECONDARY_SUBJECT_AREAS, getCoreMessages().operation_secondary_subject_areas(), false);
        contentClassifiersForm.setFields(subjectArea, secondarySubject);

        // Content Descriptors
        contentViewForm = new GroupDynamicForm(getConstants().operationContentDescriptors());
        ViewMultiLanguageTextItem description = new ViewMultiLanguageTextItem(OperationDS.DESCRIPTION, getCoreMessages().operation_description());
        ViewMultiLanguageTextItem objective = new ViewMultiLanguageTextItem(OperationDS.OBJECTIVE, getCoreMessages().operation_objective());
        contentViewForm.setFields(objective, description);

        // Class Descriptors
        classForm = new GroupDynamicForm(getConstants().operationClassDescriptors());
        ViewTextItem survey = new ViewTextItem(OperationDS.STATISTICAL_OPERATION_TYPE, getCoreMessages().operation_survey_type());
        ViewTextItem officiality = new ViewTextItem(OperationDS.OFFICIALITY_TYPE, getCoreMessages().operation_officiality_type());
        ViewTextItem indSystem = new ViewTextItem(OperationDS.INDICATOR_SYSTEM, getCoreMessages().operation_indicator_system());
        classForm.setFields(survey, officiality, indSystem);

        // Production descriptors
        productionDescriptorsForm = new GroupDynamicForm(getConstants().operationProductionDescriptors());
        ExternalItemListItem producer = new ExternalItemListItem(OperationDS.PRODUCER, getCoreMessages().operation_producer(), false);
        ViewTextItem regionalResposible = new ViewTextItem(OperationDS.REG_RESPONSIBLE, getCoreMessages().operation_regional_responsible());
        ViewTextItem regionalCont = new ViewTextItem(OperationDS.REG_CONTRIBUTOR, getCoreMessages().operation_regional_contributor());
        ViewTextItem createdDate = new ViewTextItem(OperationDS.CREATED_DATE, getConstants().operationCreatedDate());
        ViewTextItem inventoryDate = new ViewTextItem(OperationDS.INTERNAL_INVENTORY_DATE, getCoreMessages().operation_internal_inventory_date());
        ViewTextItem currentlyActive = new ViewTextItem(OperationDS.CURRENTLY_ACTIVE, getCoreMessages().operation_currently_active());
        ViewTextItem status = new ViewTextItem(OperationDS.STATUS, getCoreMessages().operation_status());
        ViewTextItem procStatus = new ViewTextItem(OperationDS.PROC_STATUS, getCoreMessages().operation_proc_status());
        productionDescriptorsForm.setFields(producer, regionalResposible, regionalCont, createdDate, inventoryDate, currentlyActive, status, procStatus);

        // Diffusion Descriptors
        diffusionForm = new GroupDynamicForm(getConstants().operationDiffusionAndPublication());
        ViewTextItem publisher = new ViewTextItem(OperationDS.PUBLISHER, getCoreMessages().operation_publisher());
        ViewMultiLanguageTextItem staticRelPolUsAc = new ViewMultiLanguageTextItem(OperationDS.RE_POL_US_AC, getCoreMessages().operation_rel_pol_us_ac());
        ViewTextItem releaseCalendar = new ViewTextItem(OperationDS.RELEASE_CALENDAR, getConstants().operationReleaseCalendar());
        ViewTextItem releaseCalendarAccess = new ViewTextItem(OperationDS.RELEASE_CALENDAR_ACCESS, getCoreMessages().operation_release_calendar_access());
        ViewTextItem updateFreq = new ViewTextItem(OperationDS.UPDATE_FREQ, getCoreMessages().operation_update_frequency());
        CustomLinkItem currentInst = new CustomLinkItem(OperationDS.CURRENT_INSTANCE, getConstants().operationCurrentInstance(), getCustomLinkItemNavigationClickHandler());
        CustomLinkItem currentInternalInst = new CustomLinkItem(OperationDS.CURRENT_INTERNAL_INSTANCE, getConstants().operationCurrentInternalInstance(), getCustomLinkItemNavigationClickHandler());
        ViewTextItem invDate = new ViewTextItem(OperationDS.INVENTORY_DATE, getCoreMessages().operation_inventory_date());
        ViewMultiLanguageTextItem staticRevPolicyItem = new ViewMultiLanguageTextItem(OperationDS.REV_POLICY, getCoreMessages().operation_rev_policy());
        ViewMultiLanguageTextItem staticRevPracticeItem = new ViewMultiLanguageTextItem(OperationDS.REV_PRACTICE, getCoreMessages().operation_rev_practice());
        ViewTextItem commonMetadata = new ViewTextItem(OperationDS.COMMON_METADATA, getCoreMessages().operation_common_metadata());
        diffusionForm.setFields(publisher, staticRelPolUsAc, releaseCalendar, releaseCalendarAccess, updateFreq, currentInst, currentInternalInst, invDate, staticRevPolicyItem, staticRevPracticeItem,
                commonMetadata);

        // Legal acts
        legalActsForm = new GroupDynamicForm(getConstants().formLegalActs());
        ViewMultiLanguageTextItem specificLegalActs = new ViewMultiLanguageTextItem(OperationDS.SPECIFIC_LEGAL_ACTS, getCoreMessages().operation_specific_legal_acts());
        ViewMultiLanguageTextItem specificDataSharing = new ViewMultiLanguageTextItem(OperationDS.SPECIFIC_DATA_SHARING, getCoreMessages().operation_specific_data_sharing());
        legalActsForm.setFields(specificLegalActs, specificDataSharing);

        // Annotations
        annotationsViewForm = new GroupDynamicForm(getConstants().operationAnnotations());
        ViewMultiLanguageTextItem staticCommentItem = new ViewMultiLanguageTextItem(OperationDS.COMMENTS, getCoreMessages().operation_comment());
        ViewMultiLanguageTextItem staticNotesItem = new ViewMultiLanguageTextItem(OperationDS.NOTES, getCoreMessages().operation_notes());
        annotationsViewForm.setFields(staticCommentItem, staticNotesItem);

        // Add to main layout
        mainFormLayout.addViewCanvas(identifiersForm);
        mainFormLayout.addViewCanvas(contentClassifiersForm);
        mainFormLayout.addViewCanvas(contentViewForm);
        mainFormLayout.addViewCanvas(classForm);
        mainFormLayout.addViewCanvas(productionDescriptorsForm);
        mainFormLayout.addViewCanvas(diffusionForm);
        mainFormLayout.addViewCanvas(legalActsForm);
        mainFormLayout.addViewCanvas(annotationsViewForm);
    }

    private void createEditionForm() {

        // IDENTIFIERS

        identifiersEditionForm = new GroupDynamicForm(getConstants().operationIdentifiers());

        RequiredTextItem code = new RequiredTextItem(OperationDS.CODE, getCoreMessages().operation_code());
        code.setShowIfCondition(new FormItemIfFunction() {

            @Override
            public boolean execute(FormItem item, Object value, DynamicForm form) {
                return canOperationCodeBeEdited();
            }
        });
        code.setValidators(CommonWebUtils.getSemanticIdentifierCustomValidator());
        ViewTextItem staticCode = new ViewTextItem(OperationDS.CODE_VIEW, getCoreMessages().operation_code());
        staticCode.setShowIfCondition(new FormItemIfFunction() {

            @Override
            public boolean execute(FormItem item, Object value, DynamicForm form) {
                return !canOperationCodeBeEdited();
            }
        });

        MultiLanguageTextItem title = new MultiLanguageTextItem(OperationDS.TITLE, getCoreMessages().operation_title());
        title.setRequired(true);
        MultiLanguageTextItem acronym = new MultiLanguageTextItem(OperationDS.ACRONYM, getCoreMessages().operation_acronym());
        ViewTextItem urn = new ViewTextItem(OperationDS.URN, getCoreMessages().operation_urn());
        identifiersEditionForm.setFields(staticCode, code, title, acronym, urn);

        // CONTENT CLASSIFIERS

        contentClassifiersEditionForm = new GroupDynamicForm(getConstants().operationContentClassifiers());
        SearchExternalItemLinkItem subjectAreaItem = createSubjectAreaItem(OperationDS.SUBJECT_AREA, getCoreMessages().operation_subject_area());
        subjectAreaItem.setRequired(true);
        SearchMultipleItemsItem secondarySubjectAreasItem = createSecondarySubjectAreasItem(OperationDS.SECONDARY_SUBJECT_AREAS, getCoreMessages().operation_secondary_subject_areas());
        contentClassifiersEditionForm.setFields(subjectAreaItem, secondarySubjectAreasItem);

        // CONTENT DESCRIPTORS

        contentEditionForm = new GroupDynamicForm(getConstants().operationContentDescriptors());
        MultilanguageRichTextEditorItem description = new MultilanguageRichTextEditorItem(OperationDS.DESCRIPTION, getCoreMessages().operation_description());
        MultilanguageRichTextEditorItem objective = new MultilanguageRichTextEditorItem(OperationDS.OBJECTIVE, getCoreMessages().operation_objective());
        // objective.setValidators(getRequiredIfInternallyPublished());
        contentEditionForm.setFields(objective, description);

        // CLASS DESCRIPTORS

        classEditionForm = new GroupDynamicForm(getConstants().operationClassDescriptors());
        surveyType = new CustomSelectItem(OperationDS.STATISTICAL_OPERATION_TYPE, getCoreMessages().operation_survey_type());
        // surveyType.setValidators(getRequiredIfInternallyPublished());
        officialityType = new CustomSelectItem(OperationDS.OFFICIALITY_TYPE, getCoreMessages().operation_officiality_type());
        // officialityType.setValidators(getRequiredIfInternallyPublished());
        indSystem = new CustomCheckboxItem(OperationDS.INDICATOR_SYSTEM, getCoreMessages().operation_indicator_system());
        indSystem.setTitleStyle("requiredFormLabel");
        classEditionForm.setFields(surveyType, officialityType, indSystem);

        // PRODUCTION DESCRIPTORS

        productionDescriptorsEditionForm = new GroupDynamicForm(getConstants().operationProductionDescriptors());

        SearchMultipleItemsItem producerItem = createProducersItem(OperationDS.PRODUCER, getCoreMessages().operation_producer());

        regionalResponsibleItem = new ExternalMultipleSelectItem(OperationDS.REG_RESPONSIBLE, getCoreMessages().operation_regional_responsible());
        regionalResponsibleItem.getSchemeItem().addChangedHandler(new ChangedHandler() {

            @Override
            public void onChanged(ChangedEvent event) {
                if (event.getValue() != null) {
                    getUiHandlers().populateRegionalResposibles(event.getValue().toString());
                }
            }
        });
        regionalContributorItem = new ExternalMultipleSelectItem(OperationDS.REG_CONTRIBUTOR, getCoreMessages().operation_regional_contributor());
        regionalContributorItem.getSchemeItem().addChangedHandler(new ChangedHandler() {

            @Override
            public void onChanged(ChangedEvent event) {
                if (event.getValue() != null) {
                    getUiHandlers().populateRegionalContributors(event.getValue().toString());
                }
            }
        });
        ViewTextItem createdDate = new ViewTextItem(OperationDS.CREATED_DATE, getConstants().operationCreatedDate());
        ViewTextItem internalInventoryDate = new ViewTextItem(OperationDS.INTERNAL_INVENTORY_DATE, getCoreMessages().operation_internal_inventory_date());
        currentlyActiveItem = new CustomCheckboxItem(OperationDS.CURRENTLY_ACTIVE, getCoreMessages().operation_currently_active());
        // currentlyActiveItem.setValidators(getRequiredIfInternallyPublished());
        statusItem = new CustomSelectItem(OperationDS.STATUS, getCoreMessages().operation_status());
        statusItem.setValueMap(CommonUtils.getStatusEnumHashMap());
        ViewTextItem procStatus = new ViewTextItem(OperationDS.PROC_STATUS, getCoreMessages().operation_proc_status());
        ViewTextItem staticProcStatus = new ViewTextItem(OperationDS.PROC_STATUS_VIEW, getCoreMessages().operation_proc_status());
        staticProcStatus.setShowIfCondition(FormItemUtils.getFalseFormItemIfFunction());
        productionDescriptorsEditionForm.setFields(producerItem, regionalResponsibleItem, regionalContributorItem, createdDate, internalInventoryDate, currentlyActiveItem, statusItem,
                staticProcStatus, procStatus);

        // DIFFUSION AND PUBLICATION

        diffusionEditionForm = new GroupDynamicForm(getConstants().operationDiffusionAndPublication());
        publisherItem = new ExternalMultipleSelectItem(OperationDS.PUBLISHER, getCoreMessages().operation_publisher());
        // publisherItem.setValidators(getRequiredIfInternallyPublished());
        publisherItem.getSchemeItem().addChangedHandler(new ChangedHandler() {

            @Override
            public void onChanged(ChangedEvent event) {
                if (event.getValue() != null) {
                    getUiHandlers().populatePublishers(event.getValue().toString());
                }
            }
        });
        MultilanguageRichTextEditorItem relPolUsAc = new MultilanguageRichTextEditorItem(OperationDS.RE_POL_US_AC, getCoreMessages().operation_rel_pol_us_ac());
        releaseCalendar = new CustomCheckboxItem(OperationDS.RELEASE_CALENDAR, getConstants().operationReleaseCalendar());
        releaseCalendarAccess = new CustomTextItem(OperationDS.RELEASE_CALENDAR_ACCESS, getCoreMessages().operation_release_calendar_access());
        releaseCalendarAccess.setValidators(CommonWebUtils.getUrlValidator());
        updateFrequencyItem = new CustomSelectItem(OperationDS.UPDATE_FREQ, getCoreMessages().operation_update_frequency());
        updateFrequencyItem.setMultiple(true);
        CustomLinkItem currentInst = new CustomLinkItem(OperationDS.CURRENT_INSTANCE, getConstants().operationCurrentInstance(), getCustomLinkItemNavigationClickHandler());
        CustomLinkItem currentInternalInst = new CustomLinkItem(OperationDS.CURRENT_INTERNAL_INSTANCE, getConstants().operationCurrentInternalInstance(), getCustomLinkItemNavigationClickHandler());
        ViewTextItem invDate = new ViewTextItem(OperationDS.INVENTORY_DATE, getCoreMessages().operation_inventory_date());
        MultilanguageRichTextEditorItem revPolicyItem = new MultilanguageRichTextEditorItem(OperationDS.REV_POLICY, getCoreMessages().operation_rev_policy());
        MultilanguageRichTextEditorItem revPracticeItem = new MultilanguageRichTextEditorItem(OperationDS.REV_PRACTICE, getConstants().operationRevPractice());
        commonMetadataItem = new CustomSelectItem(OperationDS.COMMON_METADATA, getCoreMessages().operation_common_metadata());
        // commonMetadataItem.setValidators(getRequiredIfInternallyPublished());
        diffusionEditionForm.setFields(publisherItem, relPolUsAc, releaseCalendar, releaseCalendarAccess, updateFrequencyItem, currentInst, currentInternalInst, invDate, revPolicyItem,
                revPracticeItem, commonMetadataItem);

        // LEGAL ACTS

        legalActsEditionForm = new GroupDynamicForm(getConstants().formLegalActs());
        MultilanguageRichTextEditorItem specificLegalActs = new MultilanguageRichTextEditorItem(OperationDS.SPECIFIC_LEGAL_ACTS, getCoreMessages().operation_specific_legal_acts());
        MultilanguageRichTextEditorItem specificDataSharing = new MultilanguageRichTextEditorItem(OperationDS.SPECIFIC_DATA_SHARING, getCoreMessages().operation_specific_data_sharing());
        legalActsEditionForm.setFields(specificLegalActs, specificDataSharing);

        // ANNOTATIONS

        annotationsEditionForm = new GroupDynamicForm(getConstants().operationAnnotations());
        MultilanguageRichTextEditorItem commentItem = new MultilanguageRichTextEditorItem(OperationDS.COMMENTS, getCoreMessages().operation_comment());
        MultilanguageRichTextEditorItem notesItem = new MultilanguageRichTextEditorItem(OperationDS.NOTES, getCoreMessages().operation_notes());
        annotationsEditionForm.setFields(commentItem, notesItem);

        // Add to main layout
        mainFormLayout.addEditionCanvas(identifiersEditionForm);
        mainFormLayout.addEditionCanvas(contentClassifiersEditionForm);
        mainFormLayout.addEditionCanvas(contentEditionForm);
        mainFormLayout.addEditionCanvas(classEditionForm);
        mainFormLayout.addEditionCanvas(productionDescriptorsEditionForm);
        mainFormLayout.addEditionCanvas(diffusionEditionForm);
        mainFormLayout.addEditionCanvas(legalActsEditionForm);
        mainFormLayout.addEditionCanvas(annotationsEditionForm);
    }

    private void setOperationViewMode(OperationDto operationDto) {

        // IDENTIFIERS

        identifiersForm.setValue(OperationDS.CODE, operationDto.getCode());
        identifiersForm.setValue(OperationDS.TITLE, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getTitle()));
        identifiersForm.setValue(OperationDS.ACRONYM, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getAcronym()));
        identifiersForm.setValue(OperationDS.URN, operationDto.getUrn());

        // CONTENT CLASSIFIERS

        ((ExternalItemLinkItem) contentClassifiersForm.getItem(OperationDS.SUBJECT_AREA)).setExternalItem(operationDto.getSubjectArea());
        ((ExternalItemListItem) contentClassifiersForm.getItem(OperationDS.SECONDARY_SUBJECT_AREAS)).setExternalItems(operationDto.getSecondarySubjectAreas());

        // CONTENT DESCRIPTORS

        contentViewForm.setValue(OperationDS.DESCRIPTION, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getDescription()));
        contentViewForm.setValue(OperationDS.OBJECTIVE, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getObjective()));

        // CLASS DESCRIPTORS

        classForm.setValue(OperationDS.STATISTICAL_OPERATION_TYPE,
                operationDto.getSurveyType() == null ? "" : CommonWebUtils.getElementName(operationDto.getSurveyType().getIdentifier(), operationDto.getSurveyType().getDescription()));
        classForm.setValue(OperationDS.OFFICIALITY_TYPE,
                operationDto.getOfficialityType() == null ? "" : CommonWebUtils.getElementName(operationDto.getOfficialityType().getIdentifier(), operationDto.getOfficialityType().getDescription()));
        classForm.setValue(OperationDS.INDICATOR_SYSTEM, (operationDto.getIndicatorSystem() != null && operationDto.getIndicatorSystem()) ? MetamacWebCommon.getConstants().yes() : MetamacWebCommon
                .getConstants().no());

        // PRODUCTION DESCRIPTORS

        ((ExternalItemListItem) productionDescriptorsForm.getItem(OperationDS.PRODUCER)).setExternalItems(operationDto.getProducer());
        productionDescriptorsForm.setValue(OperationDS.REG_RESPONSIBLE, ExternalItemUtils.getExternalItemListToString(operationDto.getRegionalResponsible()));
        productionDescriptorsForm.setValue(OperationDS.REG_CONTRIBUTOR, ExternalItemUtils.getExternalItemListToString(operationDto.getRegionalContributor()));
        productionDescriptorsForm.setValue(OperationDS.CREATED_DATE, operationDto.getCreatedDate());
        productionDescriptorsForm.setValue(OperationDS.INTERNAL_INVENTORY_DATE, operationDto.getInternalInventoryDate());
        productionDescriptorsForm.setValue(OperationDS.CURRENTLY_ACTIVE, (operationDto.getCurrentlyActive() != null && operationDto.getCurrentlyActive())
                ? MetamacWebCommon.getConstants().yes()
                : MetamacWebCommon.getConstants().no());
        productionDescriptorsForm.setValue(OperationDS.STATUS,
                operationDto.getStatus() == null ? null : getCoreMessages().getString(getCoreMessages().statusEnum() + operationDto.getStatus().getName()));
        productionDescriptorsForm.setValue(OperationDS.PROC_STATUS, getCoreMessages().getString(getCoreMessages().procStatusEnum() + operationDto.getProcStatus().getName()));

        // DIFFUSION AND PUBLICATION

        diffusionForm.setValue(OperationDS.PUBLISHER, ExternalItemUtils.getExternalItemListToString(operationDto.getPublisher()));
        diffusionForm.setValue(OperationDS.RE_POL_US_AC, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getRelPolUsAc()));
        diffusionForm.setValue(OperationDS.RELEASE_CALENDAR, (operationDto.getReleaseCalendar() != null && operationDto.getReleaseCalendar())
                ? MetamacWebCommon.getConstants().yes()
                : MetamacWebCommon.getConstants().no());
        diffusionForm.setValue(OperationDS.RELEASE_CALENDAR_ACCESS, operationDto.getReleaseCalendarAccess());
        diffusionForm.setValue(OperationDS.UPDATE_FREQ, ExternalItemUtils.getExternalItemListToString(operationDto.getUpdateFrequency()));

        if (operationDto.getCurrentInstance() != null) {
            ((CustomLinkItem) diffusionForm.getItem(OperationDS.CURRENT_INSTANCE)).setValue(
                    CommonWebUtils.getElementName(operationDto.getCurrentInstance().getCode(), operationDto.getCurrentInstance().getTitle()),
                    PlaceRequestUtils.buildAbsoluteInstancePlaceRequest(operationDto.getCode(), operationDto.getCurrentInstance().getCode()));
        } else {
            ((CustomLinkItem) diffusionForm.getItem(OperationDS.CURRENT_INSTANCE)).clearValue();
        }

        if (operationDto.getCurrentInternalInstance() != null) {
            ((CustomLinkItem) diffusionForm.getItem(OperationDS.CURRENT_INTERNAL_INSTANCE)).setValue(
                    CommonWebUtils.getElementName(operationDto.getCurrentInternalInstance().getCode(), operationDto.getCurrentInternalInstance().getTitle()),
                    PlaceRequestUtils.buildAbsoluteInstancePlaceRequest(operationDto.getCode(), operationDto.getCurrentInternalInstance().getCode()));
        } else {
            ((CustomLinkItem) diffusionForm.getItem(OperationDS.CURRENT_INTERNAL_INSTANCE)).clearValue();
        }

        diffusionForm.setValue(OperationDS.INVENTORY_DATE, operationDto.getInventoryDate());
        diffusionForm.setValue(OperationDS.REV_POLICY, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getRevPolicy()));
        diffusionForm.setValue(OperationDS.REV_PRACTICE, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getRevPractice()));

        diffusionForm.setValue(OperationDS.COMMON_METADATA, operationDto.getCommonMetadata() != null ? ExternalItemUtils.getExternalItemName(operationDto.getCommonMetadata()) : "");

        // LEGAL ACTS
        legalActsForm.setValue(OperationDS.SPECIFIC_LEGAL_ACTS, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getSpecificLegalActs()));
        legalActsForm.setValue(OperationDS.SPECIFIC_DATA_SHARING, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getSpecificDataSharing()));

        // ANNOTATIONS

        annotationsViewForm.setValue(OperationDS.COMMENTS, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getComment()));
        annotationsViewForm.setValue(OperationDS.NOTES, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getNotes()));
    }

    private void setOperationEditionMode(OperationDto operationDto) {

        // IDENTIFIERS

        identifiersEditionForm.setValue(OperationDS.CODE, operationDto.getCode());
        identifiersEditionForm.setValue(OperationDS.CODE_VIEW, operationDto.getCode());
        identifiersEditionForm.setValue(OperationDS.TITLE, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getTitle()));
        identifiersEditionForm.setValue(OperationDS.ACRONYM, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getAcronym()));
        identifiersEditionForm.setValue(OperationDS.URN, operationDto.getUrn());

        // CONTENT CLASSIFIERS

        ((ExternalItemLinkItem) contentClassifiersEditionForm.getItem(OperationDS.SUBJECT_AREA)).setExternalItem(operationDto.getSubjectArea());
        ((ExternalItemListItem) contentClassifiersEditionForm.getItem(OperationDS.SECONDARY_SUBJECT_AREAS)).setExternalItems(operationDto.getSecondarySubjectAreas());

        // CONTENT DESCRIPTORS

        contentEditionForm.setValue(OperationDS.DESCRIPTION, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getDescription()));
        contentEditionForm.setValue(OperationDS.OBJECTIVE, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getObjective()));

        // CLASS DESCRIPTORS

        surveyType.setValue(operationDto.getSurveyType() != null ? operationDto.getSurveyType().getId() : null);
        officialityType.setValue(operationDto.getOfficialityType() != null ? operationDto.getOfficialityType().getId() : null);
        indSystem.setValue(operationDto.getIndicatorSystem() == null ? false : operationDto.getIndicatorSystem());

        // PRODUCTION DESCRIPTORS

        ((ExternalItemListItem) productionDescriptorsEditionForm.getItem(OperationDS.PRODUCER)).setExternalItems(operationDto.getProducer());
        regionalResponsibleItem.clearValue();
        regionalContributorItem.getValue();
        productionDescriptorsEditionForm.setValue(OperationDS.CREATED_DATE, operationDto.getCreatedDate());
        productionDescriptorsEditionForm.setValue(OperationDS.INTERNAL_INVENTORY_DATE, operationDto.getInternalInventoryDate());
        currentlyActiveItem.setValue(operationDto.getCurrentlyActive() != null ? operationDto.getCurrentlyActive() : false);
        statusItem.setValue(operationDto.getStatus() == null ? null : operationDto.getStatus().toString());
        productionDescriptorsEditionForm.setValue(OperationDS.PROC_STATUS, getCoreMessages().getString(getCoreMessages().procStatusEnum() + operationDto.getProcStatus().getName()));
        productionDescriptorsEditionForm.setValue(OperationDS.PROC_STATUS_VIEW, operationDto.getProcStatus().toString());

        // DIFFUSION AND PUBLICATION

        publisherItem.clearValue();
        diffusionEditionForm.setValue(OperationDS.RE_POL_US_AC, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getRelPolUsAc()));
        releaseCalendar.setValue(operationDto.getReleaseCalendar());
        releaseCalendarAccess.setValue(operationDto.getReleaseCalendarAccess());
        updateFrequencyItem.setValues(ExternalItemUtils.getExternalItemsUrns(operationDto.getUpdateFrequency()));

        if (operationDto.getCurrentInstance() != null) {
            ((CustomLinkItem) diffusionEditionForm.getItem(OperationDS.CURRENT_INSTANCE)).setValue(
                    CommonWebUtils.getElementName(operationDto.getCurrentInstance().getCode(), operationDto.getCurrentInstance().getTitle()),
                    PlaceRequestUtils.buildAbsoluteInstancePlaceRequest(operationDto.getCode(), operationDto.getCurrentInstance().getCode()));
        } else {
            ((CustomLinkItem) diffusionEditionForm.getItem(OperationDS.CURRENT_INSTANCE)).clearValue();
        }

        if (operationDto.getCurrentInternalInstance() != null) {
            ((CustomLinkItem) diffusionEditionForm.getItem(OperationDS.CURRENT_INTERNAL_INSTANCE)).setValue(
                    CommonWebUtils.getElementName(operationDto.getCurrentInternalInstance().getCode(), operationDto.getCurrentInternalInstance().getTitle()),
                    PlaceRequestUtils.buildAbsoluteInstancePlaceRequest(operationDto.getCode(), operationDto.getCurrentInternalInstance().getCode()));
        } else {
            ((CustomLinkItem) diffusionEditionForm.getItem(OperationDS.CURRENT_INTERNAL_INSTANCE)).clearValue();
        }

        diffusionEditionForm.setValue(OperationDS.INVENTORY_DATE, operationDto.getInventoryDate());
        diffusionEditionForm.setValue(OperationDS.REV_POLICY, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getRevPolicy()));
        diffusionEditionForm.setValue(OperationDS.REV_PRACTICE, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getRevPractice()));
        commonMetadataItem.setValue(operationDto.getCommonMetadata() != null ? operationDto.getCommonMetadata().getUrn() : null);

        // LEGAL ACTS
        legalActsEditionForm.setValue(OperationDS.SPECIFIC_LEGAL_ACTS, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getSpecificLegalActs()));
        legalActsEditionForm.setValue(OperationDS.SPECIFIC_DATA_SHARING, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getSpecificDataSharing()));

        // ANNOTATIONS

        annotationsEditionForm.setValue(OperationDS.COMMENTS, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getComment()));
        annotationsEditionForm.setValue(OperationDS.NOTES, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getNotes()));

        identifiersEditionForm.markForRedraw();
        productionDescriptorsEditionForm.markForRedraw();
    }

    @Override
    public HasClickHandlers getPublishOperationInternally() {
        return mainFormLayout.getPublishInternally();
    }

    @Override
    public HasClickHandlers getPublishOperationExternally() {
        return mainFormLayout.getPublishExternally();
    }

    /**
     * Select Instance in ListGrid
     * 
     * @param id
     */
    private void selectInstance(Long id) {
        if (id == null) {
            // New instance
            instanceListGridToolStrip.getDeleteButton().hide();
            instanceListGrid.deselectAllRecords();
        } else {
            showInstanceListGridDeleteButton();
        }
    }

    /**
     * DeSelect Instance in ListGrid
     */
    private void deselectInstance() {
        instanceListGridToolStrip.getDeleteButton().hide();
    }

    @Override
    public void setOrganisationSchemes(List<ExternalItemDto> schemes) {
        regionalResponsibleItem.setSchemesValueMap(ExternalItemUtils.getExternalItemsHashMap(schemes));
        regionalContributorItem.setSchemesValueMap(ExternalItemUtils.getExternalItemsHashMap(schemes));
        publisherItem.setSchemesValueMap(ExternalItemUtils.getExternalItemsHashMap(schemes));
    }

    @Override
    public void setOperationsLists(List<SurveyTypeDto> surveyTypeDtos, List<OfficialityTypeDto> officialityTypeDtos) {
        this.surveyTypeDtos = surveyTypeDtos;
        this.officialityTypeDtos = officialityTypeDtos;
        surveyType.setValueMap(OperationsListUtils.getSurveyTypeHashMap(surveyTypeDtos));
        officialityType.setValueMap(OperationsListUtils.getOfficialityTypeHashMap(officialityTypeDtos));
    }

    @Override
    public void setRegionalResposibles(List<ExternalItemDto> organisations) {
        this.regionalResponsibles = organisations;
        regionalResponsibleItem.setItemsValueMap(ExternalItemUtils.getExternalItemsHashMap(organisations));
    }

    @Override
    public void setRegionalContributors(List<ExternalItemDto> organisations) {
        this.regionalContributors = organisations;
        regionalContributorItem.setItemsValueMap(ExternalItemUtils.getExternalItemsHashMap(organisations));
    }

    @Override
    public void setPublishers(List<ExternalItemDto> organisations) {
        this.publishers = organisations;
        publisherItem.setItemsValueMap(ExternalItemUtils.getExternalItemsHashMap(organisations));
    }

    @Override
    public void setCommonMetadataConfigurations(List<ExternalItemDto> commonMetadataList) {
        this.commonMetadataList = commonMetadataList;
        commonMetadataItem.setValueMap(ExternalItemUtils.getExternalItemsHashMap(commonMetadataList));
    }

    @Override
    public void setUpdateFrequencyCodes(List<ExternalItemDto> codes) {
        this.updateFrequencyCodes = codes;
        updateFrequencyItem.setValueMap(ExternalItemUtils.getExternalItemsHashMap(codes));
    }

    private void setTranslationsShowed(boolean translationsShowed) {
        // Set translationsShowed value to international fields
        identifiersForm.setTranslationsShowed(translationsShowed);
        identifiersEditionForm.setTranslationsShowed(translationsShowed);
        contentViewForm.setTranslationsShowed(translationsShowed);
        contentEditionForm.setTranslationsShowed(translationsShowed);
        diffusionForm.setTranslationsShowed(translationsShowed);
        diffusionEditionForm.setTranslationsShowed(translationsShowed);
        annotationsViewForm.setTranslationsShowed(translationsShowed);
        annotationsEditionForm.setTranslationsShowed(translationsShowed);
    }

    private void showInstanceListGridDeleteButton() {
        if (ClientSecurityUtils.canDeleteInstance(operationDto.getCode())) {
            instanceListGridToolStrip.getDeleteButton().show();
        }
    }

    private boolean canOperationCodeBeEdited() {
        // Operation code can be edited only when ProcStatus is DRAFT
        return (productionDescriptorsEditionForm.getValue(OperationDS.PROC_STATUS_VIEW) != null && ProcStatusEnum.DRAFT.toString().equals(
                productionDescriptorsEditionForm.getValue(OperationDS.PROC_STATUS_VIEW)));
    }

    public boolean isOperationInternallyPublished() {
        return ProcStatusEnum.PUBLISH_INTERNALLY.equals(operationDto.getProcStatus());
    }

    public boolean isOperationExternallyPublished() {
        return ProcStatusEnum.PUBLISH_EXTERNALLY.equals(operationDto.getProcStatus());
    }

    @Override
    public void setFamilies(List<FamilyBaseDto> familyBaseDtos, int firstResult, int totalResults) {
        addFamiliesToOperationWindow.setFamilies(familyBaseDtos, firstResult, totalResults);
    }

    // ------------------------------------------------------------------------------------------------------------
    // EXTERNAL RESOURCES DATA SETTERS
    // ------------------------------------------------------------------------------------------------------------

    @Override
    public void setItemSchemes(String formItemName, ExternalItemsResult result) {
        if (StringUtils.equals(OperationDS.SUBJECT_AREA, formItemName)) {
            ((SearchItemItem) contentClassifiersEditionForm.getItem(formItemName)).setItemSchemes(result);

        } else if (StringUtils.equals(OperationDS.SECONDARY_SUBJECT_AREAS, formItemName)) {
            ((SearchMultipleItemsItem) contentClassifiersEditionForm.getItem(formItemName)).setItemSchemes(result);

        } else if (StringUtils.equals(OperationDS.PRODUCER, formItemName)) {
            ((SearchMultipleItemsItem) productionDescriptorsEditionForm.getItem(formItemName)).setItemSchemes(result);
        }
    }

    @Override
    public void setItems(String formItemName, ExternalItemsResult result) {
        if (StringUtils.equals(OperationDS.SUBJECT_AREA, formItemName)) {
            ((SearchItemItem) contentClassifiersEditionForm.getItem(formItemName)).setItems(result);

        } else if (StringUtils.equals(OperationDS.SECONDARY_SUBJECT_AREAS, formItemName)) {
            ((SearchMultipleItemsItem) contentClassifiersEditionForm.getItem(formItemName)).setItems(result);

        } else if (StringUtils.equals(OperationDS.PRODUCER, formItemName)) {
            ((SearchMultipleItemsItem) productionDescriptorsEditionForm.getItem(formItemName)).setItems(result);
        }
    }

    // ------------------------------------------------------------------------------------------------------------
    // EXTERNAL RESOURCES ITEMS
    // ------------------------------------------------------------------------------------------------------------

    private SearchItemItem createSubjectAreaItem(String name, String title) {
        final SearchItemItem item = new SearchCategoryItem(name, title);
        com.smartgwt.client.widgets.form.fields.events.ClickHandler clickHandler = new com.smartgwt.client.widgets.form.fields.events.ClickHandler() {

            @Override
            public void onClick(com.smartgwt.client.widgets.form.fields.events.ClickEvent event) {
                ExternalItemDto category = item.getSelectedItem();
                item.markSearchWindowForDestroy();
                ((SearchItemItem) contentClassifiersEditionForm.getItem(OperationDS.SUBJECT_AREA)).setExternalItem(category);
                contentClassifiersEditionForm.validate(false);
            }
        };
        item.setSaveClickHandler(clickHandler);
        return item;
    }

    private SearchMultipleItemsItem createSecondarySubjectAreasItem(String name, String title) {
        final SearchMultipleItemsItem item = new SearchMultipleCategoriesItem(name, title, new MultipleExternalResourceAction() {

            @Override
            public List<ExternalItemDto> getExternalItemsPreviouslySelected() {
                return new ArrayList<ExternalItemDto>(operationDto.getSecondarySubjectAreas());
            }
        });
        com.smartgwt.client.widgets.form.fields.events.ClickHandler clickHandler = new com.smartgwt.client.widgets.form.fields.events.ClickHandler() {

            @Override
            public void onClick(com.smartgwt.client.widgets.form.fields.events.ClickEvent event) {
                List<ExternalItemDto> categories = item.getSelectedItems();
                item.markSearchWindowForDestroy();
                ((SearchMultipleItemsItem) contentClassifiersEditionForm.getItem(OperationDS.SECONDARY_SUBJECT_AREAS)).setExternalItems(categories);
                contentClassifiersEditionForm.markForRedraw();
            }
        };
        item.setSaveClickHandler(clickHandler);
        return item;
    }

    private SearchMultipleItemsItem createProducersItem(String name, String title) {
        final SearchMultipleItemsItem item = new SearchMultipleOrganisationUntisItem(name, title, new MultipleExternalResourceAction() {

            @Override
            public List<ExternalItemDto> getExternalItemsPreviouslySelected() {
                return new ArrayList<ExternalItemDto>(operationDto.getProducer());
            }
        });
        com.smartgwt.client.widgets.form.fields.events.ClickHandler clickHandler = new com.smartgwt.client.widgets.form.fields.events.ClickHandler() {

            @Override
            public void onClick(com.smartgwt.client.widgets.form.fields.events.ClickEvent event) {
                List<ExternalItemDto> organisationUnits = item.getSelectedItems();
                item.markSearchWindowForDestroy();
                ((SearchMultipleItemsItem) productionDescriptorsEditionForm.getItem(OperationDS.PRODUCER)).setExternalItems(organisationUnits);
                productionDescriptorsEditionForm.markForRedraw();
            }
        };
        item.setSaveClickHandler(clickHandler);
        return item;
    }

    // ------------------------------------------------------------------------------------------------------------
    // CLICK HANDLERS
    // ------------------------------------------------------------------------------------------------------------

    private CustomLinkItemNavigationClickHandler getCustomLinkItemNavigationClickHandler() {
        return new CustomLinkItemNavigationClickHandler() {

            @Override
            public BaseUiHandlers getBaseUiHandlers() {
                return getUiHandlers();
            }
        };
    }
}
