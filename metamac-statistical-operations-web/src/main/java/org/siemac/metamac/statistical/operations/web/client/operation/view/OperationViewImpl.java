package org.siemac.metamac.statistical.operations.web.client.operation.view;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;
import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getCoreMessages;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import org.siemac.metamac.core.common.dto.serviceapi.ExternalItemBtDto;
import org.siemac.metamac.statistical.operations.core.dto.serviceapi.FamilyBaseDto;
import org.siemac.metamac.statistical.operations.core.dto.serviceapi.InstanceBaseDto;
import org.siemac.metamac.statistical.operations.core.dto.serviceapi.InstanceDto;
import org.siemac.metamac.statistical.operations.core.dto.serviceapi.OfficialityTypeDto;
import org.siemac.metamac.statistical.operations.core.dto.serviceapi.OperationDto;
import org.siemac.metamac.statistical.operations.core.dto.serviceapi.SurveyTypeDto;
import org.siemac.metamac.statistical.operations.core.enume.domain.StatusEnum;
import org.siemac.metamac.statistical.operations.web.client.model.FamilyRecord;
import org.siemac.metamac.statistical.operations.web.client.model.InstanceRecord;
import org.siemac.metamac.statistical.operations.web.client.operation.presenter.OperationPresenter;
import org.siemac.metamac.statistical.operations.web.client.operation.view.handlers.OperationUiHandlers;
import org.siemac.metamac.statistical.operations.web.client.resources.GlobalResources;
import org.siemac.metamac.statistical.operations.web.client.utils.EnumUtils;
import org.siemac.metamac.statistical.operations.web.client.utils.GopestatListUtils;
import org.siemac.metamac.statistical.operations.web.client.utils.RecordUtils;
import org.siemac.metamac.statistical.operations.web.client.widgets.AddFamiliesToOperationForm;
import org.siemac.metamac.statistical.operations.web.client.widgets.InstancesOrderFormLayout;
import org.siemac.metamac.statistical.operations.web.client.widgets.ListGridToolStrip;
import org.siemac.metamac.statistical.operations.web.client.widgets.ModalWindow;
import org.siemac.metamac.statistical.operations.web.client.widgets.NewInstanceForm;
import org.siemac.metamac.statistical.operations.web.client.widgets.PublishMainFormLayout;
import org.siemac.metamac.web.common.client.utils.ExternalItemUtils;
import org.siemac.metamac.web.common.client.utils.InternationalStringUtils;
import org.siemac.metamac.web.common.client.widgets.CustomListGrid;
import org.siemac.metamac.web.common.client.widgets.TitleLabel;
import org.siemac.metamac.web.common.client.widgets.form.GroupDynamicForm;
import org.siemac.metamac.web.common.client.widgets.form.fields.CustomCheckboxItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.ExternalMultipleSelectItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.ExternalSelectItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.MultiLanguageTextAndUrlItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.MultiLanguageTextItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.RequiredTextItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.ViewMultiLanguageTextAndUrlItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.ViewMultiLanguageTextItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.ViewTextItem;

import com.google.gwt.user.client.ui.Widget;
import com.gwtplatform.mvp.client.ViewWithUiHandlers;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.SortDirection;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.HasClickHandlers;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.HasRecordClickHandlers;
import com.smartgwt.client.widgets.grid.events.SelectionChangedHandler;
import com.smartgwt.client.widgets.grid.events.SelectionEvent;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

public class OperationViewImpl extends ViewWithUiHandlers<OperationUiHandlers> implements OperationPresenter.OperationView {

    // IDENTIFIERS
    private static final String             OP_IDENTIFIER                = "op-id";
    private static final String             OP_URI                       = "op-uri";
    private static final String             OP_TITLE                     = "op-title";
    private static final String             OP_ALTERNATIVE_TITLE         = "op-alter-title";
    // CONTENT CLASSIFIERS
    private static final String             OP_SUBJECT                   = "op-subjectItem";
    private static final String             OP_SUBJECT_SECONDARY         = "op-subjetc-secon";
    // CONTENT DESCRIPTORS
    private static final String             OP_DESCRIPTION               = "op-desc";
    private static final String             OP_OBJETIVE                  = "op-obj";
    // CLASS DESCRIPTORS
    private static final String             OP_SURVEY_TYPE               = "op-sur";
    private static final String             OP_OFFICIALITY_TYPE          = "op-off-type";
    private static final String             OP_INDICATOR_SYSTEM          = "op-ind-system";
    // PRODUCTION DESCRIPTORS
    private static final String             OP_PRODUCER                  = "op-producer";
    private static final String             OP_REG_RESPONSIBLE           = "op-reg-resp";
    private static final String             OP_REG_CONTRIBUTOR           = "op-reg-con";
    private static final String             OP_INTERNAL_INVENTORY_DATE   = "op-int-inv-date";
    private static final String             OP_CURRENTLY_ACTIVE          = "op-currently-active";
    private static final String             OP_STATUS                    = "op-status";
    private static final String             OP_PROC_STATUS               = "op-proc-status";
    // DIFUSSION DESCRIPTORS
    private static final String             OP_PUBLISHER                 = "op-publisherItem";
    private static final String             OP_RE_POL_US_AC              = "op-pol-us";
    private static final String             OP_RELEASE_CALENDAR          = "op-calendar";
    private static final String             OP_RELEASE_CALENDAR_ACCESS   = "op-calendar-access";
    private static final String             OP_UPDATE_FREQ               = "op-up-freq";
    private static final String             OP_CURRENT_INSTANCE          = "op-current-inst";
    private static final String             OP_CURRENT_INTERNAL_INSTANCE = "op-current-in-inst";
    private static final String             OP_INVENTORY_DATE            = "op-inv-date";
    private static final String             OP_REV_POLICY                = "op-rev-pol";
    private static final String             OP_REV_PRACTICE              = "op-rev-pract";
    private static final String             OP_COMMON_METADATA           = "op-com-met";
    // ANNOTATIONS
    private static final String             OP_COMMENTS                  = "op-com";
    private static final String             OP_NOTES                     = "op-not";

    private VLayout                         panel;

    private PublishMainFormLayout           mainFormLayout;

    // IDENTIFIERS
    private GroupDynamicForm                identifiersViewForm;
    private GroupDynamicForm                identifiersEditionForm;
    private RequiredTextItem                identifier;
    private MultiLanguageTextItem           title;
    private MultiLanguageTextItem           acronym;

    // CONTENT CLASSIFIERS
    private GroupDynamicForm                classificationViewForm;
    private GroupDynamicForm                classificationEditionForm;
    private ExternalSelectItem              subjectItem;
    private ExternalMultipleSelectItem      secondarySubjectItem;

    // CONTENT DESCRIPTORS
    private GroupDynamicForm                contentViewForm;
    private GroupDynamicForm                contentEditionForm;
    private MultiLanguageTextItem           objetive;
    private MultiLanguageTextItem           description;

    // CLASS DESCRIPTORS
    private GroupDynamicForm                classViewForm;
    private GroupDynamicForm                classEditionForm;
    private SelectItem                      surveyType;
    private SelectItem                      officialityType;
    private CustomCheckboxItem              indSystem;

    // PRODUCTION DESCRIPTORS
    private GroupDynamicForm                productionViewForm;
    private GroupDynamicForm                productionEditionForm;
    private ExternalMultipleSelectItem      producerItem;
    private ExternalMultipleSelectItem      regionalResponsibleItem;
    private ExternalMultipleSelectItem      regionalContributorItem;
    private CustomCheckboxItem              currentlyActiveItem;
    private SelectItem                      statusItem;

    // DIFUSSION AND PUBLICATION
    private GroupDynamicForm                diffusionViewForm;
    private GroupDynamicForm                diffusionEditionForm;
    private ExternalMultipleSelectItem      publisherItem;
    private MultiLanguageTextAndUrlItem     relPolUsAc;
    private CustomCheckboxItem              releaseCalendar;
    private TextItem                        releaseCalendarAccess;
    private SelectItem                      updateFrequencyItem;
    private MultiLanguageTextAndUrlItem     revPolicyItem;
    private MultiLanguageTextAndUrlItem     revPracticeItem;
    private SelectItem                      commonMetadataItem;

    private ViewMultiLanguageTextAndUrlItem staticRelPolUsAc;
    private ViewMultiLanguageTextAndUrlItem staticRevPolicyItem;
    private ViewMultiLanguageTextAndUrlItem staticRevPracticeItem;

    // ANNOTATIONS
    private GroupDynamicForm                annotationsViewForm;
    private GroupDynamicForm                annotationsEditionForm;
    private MultiLanguageTextAndUrlItem     commentItem;
    private MultiLanguageTextAndUrlItem     notesItem;

    private ViewMultiLanguageTextAndUrlItem staticCommentItem;
    private ViewMultiLanguageTextAndUrlItem staticNotesItem;

    private ListGridToolStrip               instanceListGridToolStrip;
    private CustomListGrid                  instanceListGrid;
    private InstancesOrderFormLayout        instancesOrderFormLayout;
    // Instance modal window
    private ModalWindow                     newInstanceWindow;
    private NewInstanceForm                 newInstanceForm;

    private ToolStrip                       familiesToolStrip;
    private ToolStripButton                 editToolStripButton;
    private ListGrid                        familyListGrid;
    // Families modal window
    private ModalWindow                     familiesWindow;
    private AddFamiliesToOperationForm      addFamiliesToOperationForm;

    private List<FamilyBaseDto>             familyBaseDtos;
    private List<FamilyBaseDto>             allFamilies;

    private List<SurveyTypeDto>             surveyTypeDtos;
    private List<OfficialityTypeDto>        officialityTypeDtos;

    private List<ExternalItemBtDto>         subjects;
    private List<ExternalItemBtDto>         secondarySubjects;
    private List<ExternalItemBtDto>         producers;
    private List<ExternalItemBtDto>         regionalResponsibles;
    private List<ExternalItemBtDto>         regionalContributors;
    private List<ExternalItemBtDto>         publishers;
    private List<ExternalItemBtDto>         commonMetadataList;
    private List<ExternalItemBtDto>         updateFrequencyCodes;

    public OperationViewImpl() {
        super();
        panel = new VLayout();

        // OPERATION

        mainFormLayout = new PublishMainFormLayout();
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
        instanceListGridToolStrip = new ListGridToolStrip(getConstants().instanceDeleteConfirmation());
        instanceListGridToolStrip.getNewButton().addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                newInstanceWindow = new ModalWindow();
                newInstanceWindow.setTitle(getConstants().actionNewInstance());
                newInstanceWindow.setAutoSize(true);
                newInstanceWindow.addItem(newInstanceForm);
                newInstanceWindow.show();
            }
        });

        instancesOrderFormLayout = new InstancesOrderFormLayout();
        instancesOrderFormLayout.setTitleLabelContents(getConstants().instances());
        instancesOrderFormLayout.getSave().addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                getUiHandlers().updateInstancesOrder(instancesOrderFormLayout.getInstancesOrder());
            }
        });

        instanceListGrid = new CustomListGrid();
        instanceListGrid.setHeight(150);
        ListGridField identifierField = new ListGridField(InstanceRecord.CODE, getConstants().instanceIdentifier());
        ListGridField titleField = new ListGridField(InstanceRecord.TITLE, getConstants().instanceTitle());
        ListGridField descriptionField = new ListGridField(InstanceRecord.DESCRIPTION, getConstants().instanceDescription());
        ListGridField statusField = new ListGridField(InstanceRecord.STATUS, getConstants().instanceStatus());
        ListGridField orderField = new ListGridField(InstanceRecord.ORDER, getConstants().instanceOrder());
        instanceListGrid.setFields(identifierField, titleField, descriptionField, statusField, orderField);
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
                        instanceListGridToolStrip.getDeleteButton().show();
                    }
                }
            }
        });

        VLayout instancesListGridLayout = new VLayout();
        instancesListGridLayout.setMargin(15);
        instancesListGridLayout.addMember(instanceListGridToolStrip);
        instancesListGridLayout.addMember(instanceListGrid);

        // FAMILIES

        addFamiliesToOperationForm = new AddFamiliesToOperationForm();
        familiesToolStrip = new ToolStrip();
        familiesToolStrip.setWidth100();
        editToolStripButton = new ToolStripButton(getConstants().actionEdit(), GlobalResources.RESOURCE.editListGrid().getURL());
        editToolStripButton.addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                familiesWindow = new ModalWindow();
                familiesWindow.setTitle(getConstants().actionAddFamiliesToOperation());
                familiesWindow.setAutoSize(true);
                addFamiliesToOperationForm.clearValues();
                addFamiliesToOperationForm.setFamiliesValueMap(allFamilies);
                addFamiliesToOperationForm.setFamilies(familyBaseDtos);
                familiesWindow.addItem(addFamiliesToOperationForm);
                familiesWindow.show();
            }
        });
        familiesToolStrip.addButton(editToolStripButton);

        TitleLabel familiesTitleLabel = new TitleLabel(getConstants().families());
        familiesTitleLabel.setStyleName("sectionTitleLeftMargin");

        familyListGrid = new ListGrid();
        familyListGrid.setHeight(150);
        ListGridField identifierFamilyField = new ListGridField(FamilyRecord.CODE, getConstants().familyIdentifier());
        ListGridField titleFamilyField = new ListGridField(FamilyRecord.TITLE, getConstants().familyTitle());
        ListGridField descriptionFamilyField = new ListGridField(FamilyRecord.DESCRIPTION, getConstants().familyDescription());
        ListGridField statusFamilyField = new ListGridField(FamilyRecord.STATUS, getConstants().familyStatus());
        familyListGrid.setFields(identifierFamilyField, titleFamilyField, descriptionFamilyField, statusFamilyField);

        VLayout familiesListGridLayout = new VLayout();
        familiesListGridLayout.setMargin(15);
        familiesListGridLayout.addMember(familiesToolStrip);
        familiesListGridLayout.addMember(familyListGrid);

        VLayout subPanel = new VLayout();
        subPanel.setHeight100();
        subPanel.setOverflow(Overflow.SCROLL);
        subPanel.addMember(mainFormLayout);

        subPanel.addMember(instancesOrderFormLayout);
        subPanel.addMember(instancesListGridLayout);

        subPanel.addMember(familiesTitleLabel);
        subPanel.addMember(familiesListGridLayout);

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
        if (slot == OperationPresenter.TYPE_SetContextAreaContentToolBar) {
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
    public HasRecordClickHandlers getSelectedFamily() {
        return familyListGrid;
    }

    @Override
    public HasRecordClickHandlers getSelectedInstance() {
        return instanceListGrid;
    }

    @Override
    public void setOperation(OperationDto operationDto, List<InstanceBaseDto> instanceBaseDtos, List<FamilyBaseDto> familyBaseDtos) {
        // Operation
        setOperation(operationDto);

        // Set Instances
        setInstances(instanceBaseDtos);

        // Set Families
        setFamilies(familyBaseDtos);
    }

    @Override
    public OperationDto getOperation(OperationDto operationDto) {
        // Identifiers
        operationDto.setCode(operationDto.getCode());
        operationDto.setTitle(title.getValue());
        operationDto.setAcronym(acronym.getValue());
        // Content classifiers
        operationDto.setSubjectArea(subjectItem.getSelectedExternalItem(subjects));
        operationDto.getSecondarySubjectAreas().clear();
        operationDto.getSecondarySubjectAreas().addAll(secondarySubjectItem.getSelectedExternalItems(secondarySubjects));
        // Content Descriptors
        operationDto.setDescription(description.getValue());
        operationDto.setObjective(objetive.getValue());

        // Class Descriptors
        operationDto.setSurveyType(GopestatListUtils.getSurveyTypeDto(surveyType.getValueAsString(), surveyTypeDtos));
        operationDto.setOfficialityType(GopestatListUtils.getOfficialityTypeDto(officialityType.getValueAsString(), officialityTypeDtos));
        operationDto.setIndicatorSystem(indSystem.getValueAsBoolean() == null ? false : indSystem.getValueAsBoolean());

        // Production descriptors
        operationDto.getProducer().clear();
        operationDto.getProducer().addAll(producerItem.getSelectedExternalItems(producers));
        operationDto.getRegionalResponsible().clear();
        operationDto.getRegionalResponsible().addAll(regionalResponsibleItem.getSelectedExternalItems(regionalResponsibles));
        operationDto.getRegionalContributor().clear();
        operationDto.getRegionalContributor().addAll(regionalContributorItem.getSelectedExternalItems(regionalContributors));
        operationDto.setCurrentlyActive(currentlyActiveItem.getValueAsBoolean());
        operationDto.setStatus(statusItem.getValueAsString() != null ? StatusEnum.valueOf(statusItem.getValueAsString()) : null);

        // Diffusion and Publication
        operationDto.getPublisher().clear();
        operationDto.getPublisher().addAll(publisherItem.getSelectedExternalItems(publishers));
        operationDto.setRelPolUsAc(relPolUsAc.getTextValue());
        operationDto.setRelPolUsAcUrl(relPolUsAc.getUrlValue());
        operationDto.setReleaseCalendar(releaseCalendar.getValueAsBoolean());
        operationDto.setReleaseCalendarAccess(releaseCalendarAccess.getValueAsString());
        operationDto.getUpdateFrequency().clear();
        operationDto.getUpdateFrequency().addAll(ExternalItemUtils.getExternalItemBtDtoListFromCodeIds(updateFrequencyCodes, updateFrequencyItem.getValues()));
        operationDto.setRevPolicy(revPolicyItem.getTextValue());
        operationDto.setRevPolicyUrl(revPolicyItem.getUrlValue());
        operationDto.setRevPractice(revPracticeItem.getTextValue());
        operationDto.setRevPracticeUrl(revPracticeItem.getUrlValue());
        operationDto.setCommonMetadata(ExternalItemUtils.getExternalItemBtDtoFromCodeId(commonMetadataList, commonMetadataItem.getValueAsString()));

        // ANNOTATIONS
        operationDto.setComment(commentItem.getTextValue());
        operationDto.setCommentUrl(commentItem.getUrlValue());
        operationDto.setNotes(notesItem.getTextValue());
        operationDto.setNotesUrl(notesItem.getUrlValue());
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
        // return identifiersEditionForm.validate(false) &&
        // productionEditionForm.validate(false) &&
        // contentEditionForm.validate(false) &&
        // classificationEditionForm.validate(false) &&
        // diffusionEditionForm.validate(false) &&
        // subjectItem.validateItem();
        return true;
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
        instanceListGrid.sort(InstanceRecord.ORDER, SortDirection.DESCENDING);

        instancesOrderFormLayout.addInstance(instanceDto);
    }

    @Override
    public boolean validateNewInstance() {
        return newInstanceForm.validate();
    }

    @Override
    public void closeInstanceWindow() {
        newInstanceWindow.destroy();
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
        instancesOrderFormLayout.setInstances(instanceBaseDtos);

        instanceListGrid.removeAllData();
        if (instanceBaseDtos != null) {
            for (InstanceBaseDto instanceBaseDto : instanceBaseDtos) {
                instanceListGrid.addData(RecordUtils.getInstanceRecord(instanceBaseDto));
            }
        }
        deselectInstance();
    }

    @Override
    public void setFamilies(List<FamilyBaseDto> familyBaseDtos) {
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
        setViewForm(operationDto);
        setEditionForm(operationDto);
    }

    private void createViewForm() {
        // Identifiers
        identifiersViewForm = new GroupDynamicForm(getConstants().operationIdentifiers());
        ViewTextItem identifier = new ViewTextItem(OP_IDENTIFIER, getConstants().operationIdentifier());
        ViewTextItem uri = new ViewTextItem(OP_URI, getConstants().operationUri());
        ViewMultiLanguageTextItem title = new ViewMultiLanguageTextItem(OP_TITLE, getConstants().operationTitle());
        ViewMultiLanguageTextItem alternativeTitle = new ViewMultiLanguageTextItem(OP_ALTERNATIVE_TITLE, getConstants().operationAcronym());
        identifiersViewForm.setFields(identifier, uri, title, alternativeTitle);

        // Content Classifiers
        classificationViewForm = new GroupDynamicForm(getConstants().operationContentClassifiers());
        ViewTextItem subject = new ViewTextItem(OP_SUBJECT, getConstants().operationSubjectArea());
        ViewTextItem secondarySubject = new ViewTextItem(OP_SUBJECT_SECONDARY, getConstants().operationSubjectAreasSecundary());
        classificationViewForm.setFields(subject, secondarySubject);

        // Content Descriptors
        contentViewForm = new GroupDynamicForm(getConstants().operationContentDescriptors());
        ViewMultiLanguageTextItem description = new ViewMultiLanguageTextItem(OP_DESCRIPTION, getConstants().operationDescription());
        ViewMultiLanguageTextItem objetive = new ViewMultiLanguageTextItem(OP_OBJETIVE, getConstants().operationObjetive());
        contentViewForm.setFields(objetive, description);

        // Class Descriptors
        classViewForm = new GroupDynamicForm(getConstants().operationClassDescriptors());
        ViewTextItem survey = new ViewTextItem(OP_SURVEY_TYPE, getConstants().operationSurveyType());
        ViewTextItem officiality = new ViewTextItem(OP_OFFICIALITY_TYPE, getConstants().operationOfficialityType());
        ViewTextItem indSystem = new ViewTextItem(OP_INDICATOR_SYSTEM, getConstants().operationIndicatorSystem());
        classViewForm.setFields(survey, officiality, indSystem);

        // Production descriptors
        productionViewForm = new GroupDynamicForm(getConstants().operationProductionDescriptors());
        ViewTextItem producer = new ViewTextItem(OP_PRODUCER, getConstants().operationProducers());
        ViewTextItem regionalResposible = new ViewTextItem(OP_REG_RESPONSIBLE, getConstants().operationRegionalResponsibles());
        ViewTextItem regionalCont = new ViewTextItem(OP_REG_CONTRIBUTOR, getConstants().operationRegionalContributors());
        ViewTextItem inventoryDate = new ViewTextItem(OP_INTERNAL_INVENTORY_DATE, getConstants().operationInternalInventoryDate());
        ViewTextItem currentlyActive = new ViewTextItem(OP_CURRENTLY_ACTIVE, getConstants().operationCurrentlyActive());
        ViewTextItem status = new ViewTextItem(OP_STATUS, getConstants().operationStatus());
        ViewTextItem procStatus = new ViewTextItem(OP_PROC_STATUS, getConstants().operationProcStatus());
        productionViewForm.setFields(producer, regionalResposible, regionalCont, inventoryDate, currentlyActive, status, procStatus);

        // Diffusion Descriptors
        diffusionViewForm = new GroupDynamicForm(getConstants().operationDiffusionAndPublication());
        ViewTextItem publisher = new ViewTextItem(OP_PUBLISHER, getConstants().operationPublisher());
        staticRelPolUsAc = new ViewMultiLanguageTextAndUrlItem(OP_RE_POL_US_AC, getConstants().operationReleaseUsersPolicy());
        ViewTextItem releaseCalendar = new ViewTextItem(OP_RELEASE_CALENDAR, getConstants().operationReleaseCalendar());
        ViewTextItem releaseCalendarAccess = new ViewTextItem(OP_RELEASE_CALENDAR_ACCESS, getConstants().operationReleaseCalendarAccess());
        ViewTextItem updateFreq = new ViewTextItem(OP_UPDATE_FREQ, getConstants().operationUpdateFrequency());
        ViewTextItem currentInst = new ViewTextItem(OP_CURRENT_INSTANCE, getConstants().operationCurrentInstance());
        ViewTextItem currentInternalInst = new ViewTextItem(OP_CURRENT_INTERNAL_INSTANCE, getConstants().operationCurrentInternalInstance());
        ViewTextItem invDate = new ViewTextItem(OP_INVENTORY_DATE, getConstants().operationInventoryDate());
        staticRevPolicyItem = new ViewMultiLanguageTextAndUrlItem(OP_REV_POLICY, getConstants().operationRevPolicy());
        staticRevPracticeItem = new ViewMultiLanguageTextAndUrlItem(OP_REV_PRACTICE, getConstants().operationRevPractice());
        ViewTextItem commonMetadata = new ViewTextItem(OP_COMMON_METADATA, getConstants().operationCommonMetadata());
        diffusionViewForm.setFields(publisher, staticRelPolUsAc, releaseCalendar, releaseCalendarAccess, updateFreq, currentInst, currentInternalInst, invDate, staticRevPolicyItem,
                staticRevPracticeItem, commonMetadata);

        // Annotations
        annotationsViewForm = new GroupDynamicForm(getConstants().operationAnnotations());
        staticCommentItem = new ViewMultiLanguageTextAndUrlItem(OP_COMMENTS, getConstants().operationComments());
        staticNotesItem = new ViewMultiLanguageTextAndUrlItem(OP_NOTES, getConstants().operationNotes());
        annotationsViewForm.setFields(staticCommentItem, staticNotesItem);

        // Add to main layout
        mainFormLayout.addViewCanvas(identifiersViewForm);
        mainFormLayout.addViewCanvas(classificationViewForm);
        mainFormLayout.addViewCanvas(contentViewForm);
        mainFormLayout.addViewCanvas(classViewForm);
        mainFormLayout.addViewCanvas(productionViewForm);
        mainFormLayout.addViewCanvas(diffusionViewForm);
        mainFormLayout.addViewCanvas(annotationsViewForm);
    }

    private void createEditionForm() {
        // Identifiers
        identifiersEditionForm = new GroupDynamicForm(getConstants().operationIdentifiers());
        identifier = new RequiredTextItem(OP_IDENTIFIER, getConstants().operationIdentifier());
        ViewTextItem uri = new ViewTextItem(OP_URI, getConstants().operationUri());
        title = new MultiLanguageTextItem(OP_TITLE, getConstants().operationTitle());
        title.setRequired(true);
        acronym = new MultiLanguageTextItem(OP_ALTERNATIVE_TITLE, getConstants().operationAcronym());
        identifiersEditionForm.setFields(identifier, uri, title, acronym);

        // Content classifiers
        classificationEditionForm = new GroupDynamicForm(getConstants().operationContentClassifiers());
        subjectItem = new ExternalSelectItem(OP_SUBJECT, getConstants().operationSubjectArea());
        subjectItem.setRequired(true);
        subjectItem.getSchemeItem().addChangedHandler(new ChangedHandler() {

            @Override
            public void onChanged(ChangedEvent event) {
                if (event.getValue() != null) {
                    getUiHandlers().populateSubjects(event.getValue().toString());
                }
            }
        });
        secondarySubjectItem = new ExternalMultipleSelectItem(OP_SUBJECT_SECONDARY, getConstants().operationSubjectAreasSecundary());
        secondarySubjectItem.getSchemeItem().addChangedHandler(new ChangedHandler() {

            @Override
            public void onChanged(ChangedEvent event) {
                if (event.getValue() != null) {
                    getUiHandlers().populateSecondarySubjects(event.getValue().toString());
                }
            }
        });
        classificationEditionForm.setFields(subjectItem, secondarySubjectItem);

        // Content Descriptors
        contentEditionForm = new GroupDynamicForm(getConstants().operationContentDescriptors());
        description = new MultiLanguageTextItem(OP_DESCRIPTION, getConstants().operationDescription());
        objetive = new MultiLanguageTextItem(OP_OBJETIVE, getConstants().operationObjetive());
        objetive.setRequired(true);
        contentEditionForm.setFields(objetive, description);

        // Class Descriptors
        classEditionForm = new GroupDynamicForm(getConstants().operationClassDescriptors());
        surveyType = new SelectItem(OP_SURVEY_TYPE, getConstants().operationSurveyType());
        officialityType = new SelectItem(OP_OFFICIALITY_TYPE, getConstants().operationOfficialityType());
        indSystem = new CustomCheckboxItem(OP_INDICATOR_SYSTEM, getConstants().operationIndicatorSystem());
        indSystem.setTitleStyle("requiredFormLabel");
        classEditionForm.setFields(surveyType, officialityType, indSystem);

        // Production descriptors
        productionEditionForm = new GroupDynamicForm(getConstants().operationProductionDescriptors());
        producerItem = new ExternalMultipleSelectItem(OP_PRODUCER, getConstants().operationProducers());
        producerItem.getSchemeItem().addChangedHandler(new ChangedHandler() {

            @Override
            public void onChanged(ChangedEvent event) {
                if (event.getValue() != null) {
                    getUiHandlers().populateProducers(event.getValue().toString());
                }
            }
        });
        regionalResponsibleItem = new ExternalMultipleSelectItem(OP_REG_RESPONSIBLE, getConstants().operationRegionalResponsibles());
        regionalResponsibleItem.getSchemeItem().addChangedHandler(new ChangedHandler() {

            @Override
            public void onChanged(ChangedEvent event) {
                if (event.getValue() != null) {
                    getUiHandlers().populateRegionalResposibles(event.getValue().toString());
                }
            }
        });
        regionalContributorItem = new ExternalMultipleSelectItem(OP_REG_CONTRIBUTOR, getConstants().operationRegionalContributors());
        regionalContributorItem.getSchemeItem().addChangedHandler(new ChangedHandler() {

            @Override
            public void onChanged(ChangedEvent event) {
                if (event.getValue() != null) {
                    getUiHandlers().populateRegionalContributors(event.getValue().toString());
                }
            }
        });
        ViewTextItem inventoryDate = new ViewTextItem(OP_INTERNAL_INVENTORY_DATE, getConstants().operationInternalInventoryDate());
        currentlyActiveItem = new CustomCheckboxItem(OP_CURRENTLY_ACTIVE, getConstants().operationCurrentlyActive());
        statusItem = new SelectItem(OP_STATUS, getConstants().operationStatus());
        statusItem.setValueMap(EnumUtils.getStatusEnumHashMap());
        ViewTextItem procStatus = new ViewTextItem(OP_PROC_STATUS, getConstants().operationProcStatus());
        productionEditionForm.setFields(producerItem, regionalResponsibleItem, regionalContributorItem, inventoryDate, currentlyActiveItem, statusItem, procStatus);

        // Diffusion and Publication
        diffusionEditionForm = new GroupDynamicForm(getConstants().operationDiffusionAndPublication());
        publisherItem = new ExternalMultipleSelectItem(OP_PUBLISHER, getConstants().operationPublisher());
        publisherItem.getSchemeItem().addChangedHandler(new ChangedHandler() {

            @Override
            public void onChanged(ChangedEvent event) {
                if (event.getValue() != null) {
                    getUiHandlers().populatePublishers(event.getValue().toString());
                }
            }
        });
        relPolUsAc = new MultiLanguageTextAndUrlItem(OP_RE_POL_US_AC, getConstants().operationReleaseUsersPolicy());
        releaseCalendar = new CustomCheckboxItem(OP_RELEASE_CALENDAR, getConstants().operationReleaseCalendar());
        releaseCalendarAccess = new TextItem(OP_RELEASE_CALENDAR_ACCESS, getConstants().operationReleaseCalendarAccess());
        updateFrequencyItem = new SelectItem(OP_UPDATE_FREQ, getConstants().operationUpdateFrequency());
        updateFrequencyItem.setMultiple(true);
        ViewTextItem currentInst = new ViewTextItem(OP_CURRENT_INSTANCE, getConstants().operationCurrentInstance());
        ViewTextItem currentInternalInst = new ViewTextItem(OP_CURRENT_INTERNAL_INSTANCE, getConstants().operationCurrentInternalInstance());
        ViewTextItem invDate = new ViewTextItem(OP_INVENTORY_DATE, getConstants().operationInventoryDate());
        revPolicyItem = new MultiLanguageTextAndUrlItem(OP_REV_POLICY, getConstants().operationRevPolicy());
        revPracticeItem = new MultiLanguageTextAndUrlItem(OP_REV_PRACTICE, getConstants().operationRevPractice());
        commonMetadataItem = new SelectItem(OP_COMMON_METADATA, getConstants().operationCommonMetadata());
        diffusionEditionForm.setFields(publisherItem, relPolUsAc, releaseCalendar, releaseCalendarAccess, updateFrequencyItem, currentInst, currentInternalInst, invDate, revPolicyItem,
                revPracticeItem, commonMetadataItem);

        // Annotations
        annotationsEditionForm = new GroupDynamicForm(getConstants().operationAnnotations());
        commentItem = new MultiLanguageTextAndUrlItem(OP_COMMENTS, getConstants().operationComments());
        notesItem = new MultiLanguageTextAndUrlItem(OP_NOTES, getConstants().operationNotes());
        annotationsEditionForm.setFields(commentItem, notesItem);

        // Add to main layout
        mainFormLayout.addEditionCanvas(identifiersEditionForm);
        mainFormLayout.addEditionCanvas(classificationEditionForm);
        mainFormLayout.addEditionCanvas(contentEditionForm);
        mainFormLayout.addEditionCanvas(classEditionForm);
        mainFormLayout.addEditionCanvas(productionEditionForm);
        mainFormLayout.addEditionCanvas(diffusionEditionForm);
        mainFormLayout.addEditionCanvas(annotationsEditionForm);
    }

    private void setViewForm(OperationDto operationDto) {
        // Identifiers
        identifiersViewForm.setValue(OP_IDENTIFIER, operationDto.getCode());
        identifiersViewForm.setValue(OP_URI, operationDto.getUri());
        identifiersViewForm.setValue(OP_TITLE, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getTitle()));
        identifiersViewForm.setValue(OP_ALTERNATIVE_TITLE, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getAcronym()));

        // Content Classifiers
        classificationViewForm.setValue(OP_SUBJECT, operationDto.getSubjectArea() == null ? "" : operationDto.getSubjectArea().getCodeId());
        classificationViewForm.setValue(OP_SUBJECT_SECONDARY, ExternalItemUtils.getExternalItemListToString(operationDto.getSecondarySubjectAreas()));

        // Content Descriptors
        contentViewForm.setValue(OP_DESCRIPTION, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getDescription()));
        contentViewForm.setValue(OP_OBJETIVE, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getObjective()));

        // Class Descriptors
        classViewForm.setValue(OP_SURVEY_TYPE, operationDto.getSurveyType() == null ? "" : operationDto.getSurveyType().getIdentifier());
        classViewForm.setValue(OP_OFFICIALITY_TYPE, operationDto.getOfficialityType() == null ? "" : operationDto.getOfficialityType().getIdentifier());
        classViewForm.setValue(OP_INDICATOR_SYSTEM, (operationDto.getIndicatorSystem() != null && operationDto.getIndicatorSystem()) ? getConstants().yes() : getConstants().no());

        // Production Descriptors
        productionViewForm.setValue(OP_PRODUCER, ExternalItemUtils.getExternalItemListToString(operationDto.getProducer()));
        productionViewForm.setValue(OP_REG_RESPONSIBLE, ExternalItemUtils.getExternalItemListToString(operationDto.getRegionalResponsible()));
        productionViewForm.setValue(OP_REG_CONTRIBUTOR, ExternalItemUtils.getExternalItemListToString(operationDto.getRegionalContributor()));
        productionViewForm.setValue(OP_INTERNAL_INVENTORY_DATE, operationDto.getInternalInventoryDate() != null ? operationDto.getInternalInventoryDate().toString() : "");
        productionViewForm.setValue(OP_CURRENTLY_ACTIVE, (operationDto.getCurrentlyActive() != null && operationDto.getCurrentlyActive()) ? getConstants().yes() : getConstants().no());
        productionViewForm.setValue(OP_STATUS, operationDto.getStatus() == null ? null : getCoreMessages().getString(getCoreMessages().statusEnum() + operationDto.getStatus().getName()));
        productionViewForm.setValue(OP_PROC_STATUS, getCoreMessages().getString(getCoreMessages().procStatusEnum() + operationDto.getProcStatus().getName()));

        // Diffusion and Publication
        diffusionViewForm.setValue(OP_PUBLISHER, ExternalItemUtils.getExternalItemListToString(operationDto.getPublisher()));
        staticRelPolUsAc.setValue(operationDto.getRelPolUsAc(), operationDto.getRelPolUsAcUrl());
        diffusionViewForm.setValue(OP_RELEASE_CALENDAR, (operationDto.getReleaseCalendar() != null && operationDto.getReleaseCalendar()) ? getConstants().yes() : getConstants().no());
        diffusionViewForm.setValue(OP_RELEASE_CALENDAR_ACCESS, operationDto.getReleaseCalendarAccess());
        diffusionViewForm.setValue(OP_UPDATE_FREQ, ExternalItemUtils.getExternalItemListToString(operationDto.getUpdateFrequency()));
        diffusionViewForm.setValue(OP_CURRENT_INSTANCE, operationDto.getCurrentInstance() != null ? operationDto.getCurrentInstance().getCode() : "");
        diffusionViewForm.setValue(OP_INVENTORY_DATE, operationDto.getInventoryDate() != null ? operationDto.getInventoryDate().toString() : "");
        staticRevPolicyItem.setValue(operationDto.getRevPolicy(), operationDto.getRevPolicyUrl());
        staticRevPracticeItem.setValue(operationDto.getRevPractice(), operationDto.getRevPracticeUrl());

        diffusionViewForm.setValue(OP_COMMON_METADATA, operationDto.getCommonMetadata() != null ? operationDto.getCommonMetadata().getCodeId() : "");

        // Annotations
        staticCommentItem.setValue(operationDto.getComment(), operationDto.getCommentUrl());
        staticNotesItem.setValue(operationDto.getNotes(), operationDto.getNotesUrl());
    }

    private void setEditionForm(OperationDto operationDto) {
        // Identifiers
        identifier.setValue(operationDto.getCode());
        identifiersEditionForm.setValue(OP_URI, operationDto.getUri());
        title.setValue(operationDto.getTitle());
        acronym.setValue(operationDto.getAcronym());

        // Content classifiers
        subjectItem.clearValue();
        // subjectItem.setItemValue(operationDto.getSubjectArea() != null ? operationDto.getSubjectArea().getCodeId() : "");
        secondarySubjectItem.clearValue();
        // secondarySubjectItem.setItemsValues(ExternalItemUtils.getExternalItemsCodeIds(operationDto.getSecundarySubjectAreas()));

        // Content Descriptors
        description.setValue(operationDto.getDescription());
        objetive.setValue(operationDto.getObjective());

        // Class Descriptors
        surveyType.setValue(operationDto.getSurveyType() != null ? operationDto.getSurveyType().getId() : null);
        officialityType.setValue(operationDto.getOfficialityType() != null ? operationDto.getOfficialityType().getId() : null);
        indSystem.setValue(operationDto.getIndicatorSystem() == null ? false : operationDto.getIndicatorSystem());

        // Production descriptors
        producerItem.clearValue();
        regionalResponsibleItem.clearValue();
        regionalContributorItem.getValue();
        productionEditionForm.setValue(OP_INTERNAL_INVENTORY_DATE, operationDto.getInternalInventoryDate() != null ? operationDto.getInternalInventoryDate().toString() : "");
        currentlyActiveItem.setValue(operationDto.getCurrentlyActive() != null ? operationDto.getCurrentlyActive() : false);
        statusItem.setValue(operationDto.getStatus() == null ? null : operationDto.getStatus().toString());
        productionEditionForm.setValue(OP_PROC_STATUS, getCoreMessages().getString(getCoreMessages().procStatusEnum() + operationDto.getProcStatus().getName()));

        // Diffusion and Publication
        publisherItem.clearValue();
        relPolUsAc.setValue(operationDto.getRelPolUsAc(), operationDto.getRelPolUsAcUrl());
        releaseCalendar.setValue(operationDto.getReleaseCalendar());
        releaseCalendarAccess.setValue(operationDto.getReleaseCalendarAccess());
        updateFrequencyItem.setValues(ExternalItemUtils.getExternalItemsCodeIds(operationDto.getUpdateFrequency()));
        diffusionEditionForm.setValue(OP_CURRENT_INSTANCE, operationDto.getCurrentInstance() != null ? operationDto.getCurrentInstance().getCode() : "");
        diffusionEditionForm.setValue(OP_INVENTORY_DATE, operationDto.getInventoryDate() != null ? operationDto.getInventoryDate().toString() : "");
        revPolicyItem.setValue(operationDto.getRevPolicy(), operationDto.getRevPolicyUrl());
        revPracticeItem.setValue(operationDto.getRevPractice(), operationDto.getRevPracticeUrl());
        commonMetadataItem.setValue(operationDto.getCommonMetadata() != null ? operationDto.getCommonMetadata().getCodeId() : null);

        // Annotations
        commentItem.setValue(operationDto.getComment(), operationDto.getCommentUrl());
        notesItem.setValue(operationDto.getNotes(), operationDto.getNotesUrl());
    }

    @Override
    public com.smartgwt.client.widgets.form.fields.events.HasClickHandlers getAddFamilies() {
        return addFamiliesToOperationForm.getAdd();
    }

    @Override
    public List<Long> getSelectedFamilyIds() {
        return addFamiliesToOperationForm.getSelectedFamilyIds();
    }

    @Override
    public boolean validateAddFamilies() {
        return addFamiliesToOperationForm.validate();
    }

    @Override
    public void closeFamiliesWindow() {
        familiesWindow.destroy();
    }

    @Override
    public void setAllFamilies(List<FamilyBaseDto> familyBaseDtos) {
        allFamilies = familyBaseDtos;
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
            instanceListGridToolStrip.getDeleteButton().show();
        }
    }

    /**
     * DeSelect Instance in ListGrid
     */
    private void deselectInstance() {
        instanceListGridToolStrip.getDeleteButton().hide();
    }

    @Override
    public void setSubjects(List<ExternalItemBtDto> subjects) {
        this.subjects = subjects;
        LinkedHashMap<String, String> subjectsMap = new LinkedHashMap<String, String>();
        for (ExternalItemBtDto subject : subjects) {
            subjectsMap.put(subject.getCodeId(), subject.getCodeId());
        }
        subjectItem.setItemsValueMap(subjectsMap);
    }

    @Override
    public void setSecondarySubjetcs(List<ExternalItemBtDto> secondarySubjects) {
        this.secondarySubjects = secondarySubjects;
        LinkedHashMap<String, String> secondarySubjectsMap = new LinkedHashMap<String, String>();
        for (ExternalItemBtDto subject : secondarySubjects) {
            secondarySubjectsMap.put(subject.getCodeId(), subject.getCodeId());
        }
        secondarySubjectItem.setItemsValueMap(secondarySubjectsMap);
    }

    @Override
    public void setCategorySchemes(List<ExternalItemBtDto> schemes) {
        LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
        for (ExternalItemBtDto scheme : schemes) {
            map.put(scheme.getCodeId(), scheme.getCodeId());
        }
        subjectItem.setSchemesValueMap(map);
        secondarySubjectItem.setSchemesValueMap(map);
    }

    @Override
    public void setOrganisationSchemes(List<ExternalItemBtDto> schemes) {
        producerItem.setSchemesValueMap(ExternalItemUtils.getExternalItemsHashMap(schemes));
        regionalResponsibleItem.setSchemesValueMap(ExternalItemUtils.getExternalItemsHashMap(schemes));
        regionalContributorItem.setSchemesValueMap(ExternalItemUtils.getExternalItemsHashMap(schemes));
        publisherItem.setSchemesValueMap(ExternalItemUtils.getExternalItemsHashMap(schemes));
    }

    @Override
    public void setGopestatLists(List<SurveyTypeDto> surveyTypeDtos, List<OfficialityTypeDto> officialityTypeDtos) {
        this.surveyTypeDtos = surveyTypeDtos;
        this.officialityTypeDtos = officialityTypeDtos;
        surveyType.setValueMap(GopestatListUtils.getSurveyTypeHashMap(surveyTypeDtos));
        officialityType.setValueMap(GopestatListUtils.getOfficialityTypeHashMap(officialityTypeDtos));
    }

    @Override
    public void setProducers(List<ExternalItemBtDto> organisations) {
        this.producers = organisations;
        producerItem.setItemsValueMap(ExternalItemUtils.getExternalItemsHashMap(organisations));
    }

    @Override
    public void setRegionalResposibles(List<ExternalItemBtDto> organisations) {
        this.regionalResponsibles = organisations;
        regionalResponsibleItem.setItemsValueMap(ExternalItemUtils.getExternalItemsHashMap(organisations));
    }

    @Override
    public void setRegionalContributors(List<ExternalItemBtDto> organisations) {
        this.regionalContributors = organisations;
        regionalContributorItem.setItemsValueMap(ExternalItemUtils.getExternalItemsHashMap(organisations));
    }

    @Override
    public void setPublishers(List<ExternalItemBtDto> organisations) {
        this.publishers = organisations;
        publisherItem.setItemsValueMap(ExternalItemUtils.getExternalItemsHashMap(organisations));
    }

    @Override
    public void setCommonMetadataList(List<ExternalItemBtDto> commonMetadataList) {
        this.commonMetadataList = commonMetadataList;
        commonMetadataItem.setValueMap(ExternalItemUtils.getExternalItemsHashMap(commonMetadataList));
    }

    @Override
    public void setUpdateFrequencyCodes(List<ExternalItemBtDto> codes) {
        this.updateFrequencyCodes = codes;
        updateFrequencyItem.setValueMap(ExternalItemUtils.getExternalItemsHashMap(codes));
    }

    private void setTranslationsShowed(boolean translationsShowed) {
        // Set translationsShowed value to international fields
        identifiersViewForm.setTranslationsShowed(translationsShowed);
        identifiersEditionForm.setTranslationsShowed(translationsShowed);
        contentViewForm.setTranslationsShowed(translationsShowed);
        contentEditionForm.setTranslationsShowed(translationsShowed);
        diffusionViewForm.setTranslationsShowed(translationsShowed);
        diffusionEditionForm.setTranslationsShowed(translationsShowed);
        annotationsViewForm.setTranslationsShowed(translationsShowed);
        annotationsEditionForm.setTranslationsShowed(translationsShowed);
    }

}
