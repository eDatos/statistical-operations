package org.siemac.metamac.statistical.operations.rest.internal.v1_0.mapper;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Set;

import org.joda.time.DateTime;
import org.siemac.metamac.core.common.bt.domain.ExternalItemBt;
import org.siemac.metamac.core.common.exception.CommonServiceExceptionType;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.core.common.exception.MetamacExceptionItem;
import org.siemac.metamac.core.common.vo.domain.ExternalItem;
import org.siemac.metamac.domain.statistical.operations.enume.domain.ProcStatusEnum;
import org.siemac.metamac.rest.common.v1_0.domain.Error;
import org.siemac.metamac.rest.common.v1_0.domain.ErrorItem;
import org.siemac.metamac.rest.common.v1_0.domain.InternationalString;
import org.siemac.metamac.rest.common.v1_0.domain.Link;
import org.siemac.metamac.rest.common.v1_0.domain.LocalisedString;
import org.siemac.metamac.rest.common.v1_0.domain.RelatedResource;
import org.siemac.metamac.statistical.operations.core.domain.CollMethod;
import org.siemac.metamac.statistical.operations.core.domain.Cost;
import org.siemac.metamac.statistical.operations.core.domain.OfficialityType;
import org.siemac.metamac.statistical.operations.core.domain.SurveySource;
import org.siemac.metamac.statistical.operations.core.domain.SurveyType;
import org.siemac.metamac.statistical.operations.rest.internal.RestInternalConstants;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Family;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Instance;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;
import org.springframework.stereotype.Component;
import org.springframework.web.util.UriComponentsBuilder;

@Component
public class Do2RestInternalMapperV10Impl implements Do2RestInternalMapperV10 {

    // @Context
    // private MessageContext context; // Always null in this bean (not in Service)...

    @Override
    public Operation toOperation(org.siemac.metamac.statistical.operations.core.domain.Operation source, String apiUrl) {
        if (source == null) {
            return null;
        }
        Operation target = new Operation();
        target.setId(source.getCode());
        target.setKind(RestInternalConstants.KIND_OPERATION);
        target.setLink(toOperationLink(source, apiUrl));
        target.setTitle(toInternationalString(source.getTitle()));
        target.setAcronym(toInternationalString(source.getAcronym()));
        target.getFamilies().addAll(familiesToRelatedResources(source.getFamilies(), apiUrl));
        target.setSubjectArea(externalItemToRelatedResource(source.getSubjectArea()));
        target.getSecondarySubjectAreas().addAll(externalItemsToRelatedResources(source.getSecondarySubjectAreas()));
        target.setObjective(toInternationalString(source.getObjective()));
        target.setDescription(toInternationalString(source.getDescription()));
        target.getInstances().addAll(instancesToRelatedResources(source.getInstances(), apiUrl));
        target.setSurveyType(surveyTypeToRelatedResource(source.getSurveyType()));
        target.setOfficialityType(officialityTypeToRelatedResource(source.getOfficialityType()));
        target.setIndicatorSystem(source.getIndicatorSystem());
        target.getProducers().addAll(externalItemsToRelatedResources(source.getProducer()));
        target.getRegionalResponsibles().addAll(externalItemsToRelatedResources(source.getRegionalResponsible()));
        target.getRegionalContributors().addAll(externalItemsToRelatedResources(source.getRegionalContributor()));
        target.setInternalInventoryDate(toDate(source.getInternalInventoryDate()));
        target.setCurrentlyActive(source.getCurrentlyActive());
        target.setStatus(source.getStatus() != null ? source.getStatus().name() : null);
        target.setProcStatus(source.getProcStatus().name());
        target.getPublishers().addAll(externalItemsToRelatedResources(source.getPublisher()));
        target.setRelPolUsAc(toInternationalString(source.getRelPolUsAc()));
        target.setRelPolUsAcUrl(source.getRelPolUsAcUrl());
        target.setReleaseCalendar(source.getReleaseCalendar());
        target.setReleaseCalendarAccess(source.getReleaseCalendarAccess());
        target.getUpdateFrequencies().addAll(externalItemsToRelatedResources(source.getUpdateFrequency()));
        target.setCurrentInstance(instanceToRelatedResource(getInstanceInProcStatus(source.getInstances(), ProcStatusEnum.PUBLISH_EXTERNALLY), apiUrl));
        target.setCurrentInternalInstance(instanceToRelatedResource(getInstanceInProcStatus(source.getInstances(), ProcStatusEnum.PUBLISH_INTERNALLY), apiUrl));
        target.setInventoryDate(toDate(source.getInventoryDate()));
        target.setRevPolicy(toInternationalString(source.getRevPolicy()));
        target.setRevPolicyUrl(source.getRevPolicyUrl());
        target.setRevPractice(toInternationalString(source.getRevPractice()));
        target.setRevPracticeUrl(source.getRevPracticeUrl());
        // TODO LEGAL_ACTS, DATA_SHARING, CONFIDENTIALITY_POLICY, CONFIDENTIALITY_DATA_TREATMENT. No están en OperationBase
        target.setComment(toInternationalString(source.getComment()));
        target.setCommentUrl(source.getCommentUrl());
        target.setNotes(toInternationalString(source.getNotes()));
        target.setNotesUrl(source.getNotesUrl());
        target.setParent(toOperationParent(apiUrl));
        target.getchildren().addAll(toOperationChildren(source, apiUrl));
        return target;
    }

    @Override
    public Family toFamily(org.siemac.metamac.statistical.operations.core.domain.Family source, String apiUrl) {
        if (source == null) {
            return null;
        }
        Family target = new Family();
        target.setId(source.getCode());
        target.setKind(RestInternalConstants.KIND_FAMILY);
        target.setLink(toFamilyLink(source, apiUrl));
        target.setTitle(toInternationalString(source.getTitle()));
        target.setAcronym(toInternationalString(source.getAcronym()));
        target.setDescription(toInternationalString(source.getDescription()));
        target.setInternalInventoryDate(toDate(source.getInternalInventoryDate()));
        target.setProcStatus(source.getProcStatus().name());
        target.setInventoryDate(toDate(source.getInventoryDate()));
        target.setParent(toFamilyParent(apiUrl));
        target.getchildren().addAll(toFamilyChildren(source, apiUrl));
        return target;
    }

    @Override
    public Instance toInstance(org.siemac.metamac.statistical.operations.core.domain.Instance source, String apiUrl) {
        if (source == null) {
            return null;
        }
        Instance target = new Instance();
        target.setId(source.getCode());
        target.setKind(RestInternalConstants.KIND_INSTANCE);
        target.setLink(toInstanceLink(source, apiUrl));
        target.setTitle(toInternationalString(source.getTitle()));
        target.setAcronym(toInternationalString(source.getAcronym()));
        target.setSurvey(operationToRelatedResource(source.getOperation(), apiUrl));
        // TODO SUCCESSOR, PREDECESSOR
        target.setDataDescription(toInternationalString(source.getDataDescription()));
        target.setStatisticalPopulation(toInternationalString(source.getStatisticalPopulation()));
        target.getStatisticalUnits().addAll(externalItemsToRelatedResources(source.getStatisticalUnit()));
        target.setGeographicGranularity(externalItemToRelatedResource(source.getGeographicGranularity()));
        target.setGeographicComparability(toInternationalString(source.getGeographicComparability()));
        target.setTemporalGranularity(externalItemToRelatedResource(source.getTemporalGranularity()));
        target.setTemporalComparability(toInternationalString(source.getTemporalComparability()));
        target.setBasePeriod(source.getBasePeriod());
        target.getUnitMeasures().addAll(externalItemsToRelatedResources(source.getUnitMeasure()));
        target.setStatConcDef(toInternationalString(source.getStatConcDef()));
        target.getStatConcDefLists().addAll(externalItemsToRelatedResources(source.getStatConcDefList()));
        target.setClassSystem(toInternationalString(source.getClassSystem()));
        target.getClassSystemLists().addAll(externalItemsToRelatedResources(source.getClassSystemList()));
        // TODO INSTANCE_TYPES
        target.setInternalInventoryDate(toDate(source.getInternalInventoryDate()));
        target.setProcStatus(source.getProcStatus().name());
        target.setDocMethod(toInternationalString(source.getDocMethod()));
        target.setSurveySource(surveySourceToRelatedResource(source.getSurveySource()));
        target.setCollMethod(collMethodToRelatedResource(source.getCollMethod()));
        target.getInformationSuppliers().addAll(externalItemsToRelatedResources(source.getInformationSuppliers()));
        target.getFreqColls().addAll(externalItemsToRelatedResources(source.getFreqColl()));
        target.setDataValidation(toInternationalString(source.getDataValidation()));
        target.setDataCompilation(toInternationalString(source.getDataCompilation()));
        target.setAdjustment(toInternationalString(source.getAdjustment()));
        target.setCostBurden(toInternationalString(source.getCostBurden()));
        target.getCosts().addAll(costToRelatedResources(source.getCost()));
        target.setQualityDoc(toInternationalString(source.getQualityDoc()));
        target.setQualityAssure(toInternationalString(source.getQualityAssure()));
        target.setQualityAssmnt(toInternationalString(source.getQualityAssmnt()));
        target.setUserNeeds(toInternationalString(source.getUserNeeds()));
        target.setUserSat(toInternationalString(source.getUserSat()));
        target.setCompleteness(toInternationalString(source.getCompleteness()));
        target.setTimeliness(toInternationalString(source.getTimeliness()));
        target.setPunctuality(toInternationalString(source.getPunctuality()));
        target.setAccuracyOverall(toInternationalString(source.getAccuracyOverall()));
        target.setSamplingErr(toInternationalString(source.getSamplingErr()));
        target.setNonsamplingErr(toInternationalString(source.getNonsamplingErr()));
        target.setCoherXDom(toInternationalString(source.getCoherXDomain()));
        target.setCoherInternal(toInternationalString(source.getCoherInternal()));
        target.setComment(toInternationalString(source.getComment()));
        target.setNotes(toInternationalString(source.getNotes()));
        target.setParent(toInstanceParent(source, apiUrl));
        target.getchildren().addAll(toInstanceChildren(source, apiUrl));
        return target;
    }

    // TODO pasar a librería común toError? Si se crea metamac-api-domain sólo con clases de Interfaz
    @Override
    public Error toError(Exception exception) {
        Error error = new Error();
        error.getErrorItems().addAll(toErrorItems(exception));
        return error;
    }

    private List<ErrorItem> toErrorItems(Exception exception) {

        List<ErrorItem> errorItems = new ArrayList<ErrorItem>();
        if (exception instanceof MetamacException) {
            MetamacException metamacException = (MetamacException) exception;
            for (MetamacExceptionItem metamacExceptionItem : metamacException.getExceptionItems()) {
                ErrorItem errorItem = new ErrorItem();
                errorItem.setCode(metamacExceptionItem.getCode());
                errorItem.setMessage(metamacExceptionItem.getMessage());
                if (metamacExceptionItem.getMessageParameters() != null) {
                    for (int i = 0; i < metamacExceptionItem.getMessageParameters().length; i++) {
                        Serializable messageParameter = metamacExceptionItem.getMessageParameters()[i];
                        String parameter = null;
                        if (messageParameter instanceof String) {
                            parameter = messageParameter.toString();
                        } else if (messageParameter instanceof String[]) {
                            parameter = Arrays.deepToString((String[]) messageParameter);
                        } else {
                            parameter = messageParameter.toString();
                        }
                        errorItem.getParameters().add(parameter);
                    }
                }
                errorItems.add(errorItem);
            }
        } else {
            ErrorItem errorItem = new ErrorItem();
            errorItem.setCode(CommonServiceExceptionType.UNKNOWN.getCode());
            errorItem.setMessage(exception.getMessage());
            errorItems.add(errorItem);
        }
        return errorItems;
    }

    private RelatedResource operationToRelatedResource(org.siemac.metamac.statistical.operations.core.domain.Operation source, String apiUrl) {
        if (source == null) {
            return null;
        }
        RelatedResource target = new RelatedResource();
        target.setId(source.getCode());
        target.setKind(RestInternalConstants.KIND_OPERATION);
        target.setLink(toOperationLink(source, apiUrl));
        target.setTitle(toInternationalString(source.getTitle()));
        return target;
    }

    private List<RelatedResource> familiesToRelatedResources(Set<org.siemac.metamac.statistical.operations.core.domain.Family> sources, String apiUrl) {
        List<RelatedResource> targets = new ArrayList<RelatedResource>();
        if (sources == null) {
            return targets;
        }
        for (org.siemac.metamac.statistical.operations.core.domain.Family source : sources) {
            RelatedResource target = familyToRelatedResource(source, apiUrl);
            targets.add(target);
        }
        return targets;
    }

    private RelatedResource familyToRelatedResource(org.siemac.metamac.statistical.operations.core.domain.Family source, String apiUrl) {
        if (source == null) {
            return null;
        }
        RelatedResource target = new RelatedResource();
        target.setId(source.getCode());
        target.setKind(RestInternalConstants.KIND_FAMILY);
        target.setLink(toFamilyLink(source, apiUrl));
        target.setTitle(toInternationalString(source.getTitle()));
        return target;
    }

    private List<RelatedResource> instancesToRelatedResources(List<org.siemac.metamac.statistical.operations.core.domain.Instance> sources, String apiUrl) {
        List<RelatedResource> targets = new ArrayList<RelatedResource>();
        if (sources == null) {
            return targets;
        }
        for (org.siemac.metamac.statistical.operations.core.domain.Instance source : sources) {
            RelatedResource target = instanceToRelatedResource(source, apiUrl);
            targets.add(target);
        }
        return targets;
    }

    private RelatedResource instanceToRelatedResource(org.siemac.metamac.statistical.operations.core.domain.Instance source, String apiUrl) {
        if (source == null) {
            return null;
        }
        RelatedResource target = new RelatedResource();
        target.setId(source.getCode());
        target.setKind(RestInternalConstants.KIND_INSTANCE);
        target.setLink(toInstanceLink(source, apiUrl));
        target.setTitle(toInternationalString(source.getTitle()));
        return target;
    }

    private RelatedResource surveyTypeToRelatedResource(SurveyType source) {
        if (source == null) {
            return null;
        }
        RelatedResource target = new RelatedResource();
        target.setId(source.getIdentifier());
        target.setTitle(toInternationalString(source.getDescription()));
        return target;
    }

    private RelatedResource officialityTypeToRelatedResource(OfficialityType source) {
        if (source == null) {
            return null;
        }
        RelatedResource target = new RelatedResource();
        target.setId(source.getIdentifier());
        target.setTitle(toInternationalString(source.getDescription()));
        return target;
    }

    private RelatedResource surveySourceToRelatedResource(SurveySource source) {
        if (source == null) {
            return null;
        }
        RelatedResource target = new RelatedResource();
        target.setId(source.getIdentifier());
        target.setTitle(toInternationalString(source.getDescription()));
        return target;
    }

    private RelatedResource collMethodToRelatedResource(CollMethod source) {
        if (source == null) {
            return null;
        }
        RelatedResource target = new RelatedResource();
        target.setId(source.getIdentifier());
        target.setTitle(toInternationalString(source.getDescription()));
        return target;
    }

    private RelatedResource costToRelatedResource(Cost source) {
        if (source == null) {
            return null;
        }
        RelatedResource target = new RelatedResource();
        target.setId(source.getIdentifier());
        target.setTitle(toInternationalString(source.getDescription()));
        return target;
    }

    private List<RelatedResource> costToRelatedResources(Set<Cost> sources) {
        List<RelatedResource> targets = new ArrayList<RelatedResource>();
        if (sources == null) {
            return targets;
        }
        for (Cost source : sources) {
            RelatedResource target = costToRelatedResource(source);
            targets.add(target);
        }
        return targets;
    }

    private List<RelatedResource> externalItemsToRelatedResources(Set<ExternalItem> sources) {
        List<RelatedResource> targets = new ArrayList<RelatedResource>();
        if (sources == null) {
            return targets;
        }
        for (ExternalItem source : sources) {
            RelatedResource target = externalItemToRelatedResource(source.getExt());
            targets.add(target);
        }
        return targets;
    }

    private RelatedResource externalItemToRelatedResource(ExternalItemBt source) {
        if (source == null) {
            return null;
        }
        RelatedResource target = new RelatedResource();
        target.setId(source.getCodeId()); // TODO próximamente se cambiará por urn en ExternalItem
        target.setKind(source.getType().name());
        target.setLink(toExternalItemLink(source));
        target.setTitle(null); // TODO se añadirá Title a ExternalItem
        return target;
    }

    private RelatedResource toOperationParent(String apiUrl) {
        RelatedResource target = new RelatedResource();
        target.setKind(RestInternalConstants.KIND_OPERATIONS);
        target.setLink(toOperationsLink(apiUrl));
        return target;
    }

    private List<RelatedResource> toOperationChildren(org.siemac.metamac.statistical.operations.core.domain.Operation operation, String apiUrl) {
        List<RelatedResource> targets = new ArrayList<RelatedResource>();

        // Instances
        RelatedResource instancesTarget = new RelatedResource();
        instancesTarget.setKind(RestInternalConstants.KIND_INSTANCES);
        instancesTarget.setLink(toInstancesLink(operation, apiUrl));
        targets.add(instancesTarget);

        // Families
        RelatedResource familiesTarget = new RelatedResource();
        familiesTarget.setKind(RestInternalConstants.KIND_FAMILIES);
        familiesTarget.setLink(toOperationChildrenFamiliesLink(operation, apiUrl));
        targets.add(familiesTarget);

        return targets;
    }

    private RelatedResource toFamilyParent(String apiUrl) {
        RelatedResource target = new RelatedResource();
        target.setKind(RestInternalConstants.KIND_FAMILIES);
        target.setLink(toFamiliesLink(apiUrl));
        return target;
    }

    private List<RelatedResource> toFamilyChildren(org.siemac.metamac.statistical.operations.core.domain.Family family, String apiUrl) {
        List<RelatedResource> targets = new ArrayList<RelatedResource>();

        // Operations of family
        RelatedResource operationsTarget = new RelatedResource();
        operationsTarget.setKind(RestInternalConstants.KIND_OPERATIONS);
        operationsTarget.setLink(toFamilyChildrenOperationsLink(family, apiUrl));
        targets.add(operationsTarget);

        return targets;
    }

    private RelatedResource toInstanceParent(org.siemac.metamac.statistical.operations.core.domain.Instance instance, String apiUrl) {
        RelatedResource target = new RelatedResource();
        target.setKind(RestInternalConstants.KIND_OPERATION);
        target.setLink(toOperationLink(instance.getOperation(), apiUrl));
        return target;
    }

    private List<RelatedResource> toInstanceChildren(org.siemac.metamac.statistical.operations.core.domain.Instance instance, String apiUrl) {
        List<RelatedResource> targets = new ArrayList<RelatedResource>();
        // No children
        return targets;
    }

    private InternationalString toInternationalString(org.siemac.metamac.core.common.ent.domain.InternationalString sources) {
        if (sources == null) {
            return null;
        }
        InternationalString targets = new InternationalString();
        for (org.siemac.metamac.core.common.ent.domain.LocalisedString source : sources.getTexts()) {
            LocalisedString target = new LocalisedString();
            target.setLabel(source.getLabel());
            target.setLocale(source.getLocale());
            targets.getTexts().add(target);
        }
        return targets;
    }

    private Date toDate(DateTime source) {
        if (source == null) {
            return null;
        }
        return source.toDate();
    }

    private Link toOperationLink(org.siemac.metamac.statistical.operations.core.domain.Operation operation, String apiUrl) {
        Link link = new Link();
        link.setRel(RestInternalConstants.LINK_SELF);
        link.setHref(createLinkHrefOperation(apiUrl, operation));
        return link;
    }

    private Link toOperationsLink(String apiUrl) {
        Link link = new Link();
        link.setRel(RestInternalConstants.LINK_SELF);
        link.setHref(createLinkHrefOperations(apiUrl));
        return link;
    }

    private Link toOperationChildrenFamiliesLink(org.siemac.metamac.statistical.operations.core.domain.Operation operation, String apiUrl) {
        Link link = new Link();
        link.setRel(RestInternalConstants.LINK_SELF);
        link.setHref(createLinkHrefOperationChildrenFamilies(apiUrl, operation));
        return link;
    }

    private Link toFamilyChildrenOperationsLink(org.siemac.metamac.statistical.operations.core.domain.Family family, String apiUrl) {
        Link link = new Link();
        link.setRel(RestInternalConstants.LINK_SELF);
        link.setHref(createLinkHrefFamilyChildrenOperations(apiUrl, family));
        return link;
    }

    private Link toFamilyLink(org.siemac.metamac.statistical.operations.core.domain.Family family, String apiUrl) {
        Link link = new Link();
        link.setRel(RestInternalConstants.LINK_SELF);
        link.setHref(createLinkHrefFamily(apiUrl, family));
        return link;
    }

    private Link toFamiliesLink(String apiUrl) {
        Link link = new Link();
        link.setRel(RestInternalConstants.LINK_SELF);
        link.setHref(createLinkHrefFamilies(apiUrl));
        return link;
    }

    private Link toInstancesLink(org.siemac.metamac.statistical.operations.core.domain.Operation operation, String apiUrl) {
        Link link = new Link();
        link.setRel(RestInternalConstants.LINK_SELF);
        link.setHref(createLinkHrefInstances(apiUrl, operation));
        return link;
    }

    private Link toInstanceLink(org.siemac.metamac.statistical.operations.core.domain.Instance instance, String apiUrl) {
        Link link = new Link();
        link.setRel(RestInternalConstants.LINK_SELF);
        link.setHref(createLinkHrefInstance(apiUrl, instance));
        return link;
    }

    private Link toExternalItemLink(ExternalItemBt externalItemBt) {
        Link link = new Link();
        link.setRel(RestInternalConstants.LINK_SELF);
        link.setHref(externalItemBt.getUriInt());
        return link;
    }

    // API/operations
    private String createLinkHrefOperations(String apiUrl) {
        return createLinkHref(apiUrl, RestInternalConstants.LINK_SUBPATH_OPERATIONS);
    }

    // API/operations/OPERATION_ID
    private String createLinkHrefOperation(String apiUrl, org.siemac.metamac.statistical.operations.core.domain.Operation operation) {
        String linkOperations = createLinkHrefOperations(apiUrl);
        return createLinkHref(linkOperations, operation.getCode());
    }

    // API/operations/OPERATION_ID/instances
    private String createLinkHrefInstances(String apiUrl, org.siemac.metamac.statistical.operations.core.domain.Operation operation) {
        String linkOperation = createLinkHrefOperation(apiUrl, operation);
        return createLinkHref(linkOperation, RestInternalConstants.LINK_SUBPATH_INSTANCES);
    }

    // API/operations/OPERATION_ID/instances/INSTANCE_ID
    private String createLinkHrefInstance(String apiUrl, org.siemac.metamac.statistical.operations.core.domain.Instance instance) {
        String linkOperation = createLinkHrefInstances(apiUrl, instance.getOperation());
        return createLinkHref(linkOperation, instance.getCode());
    }

    // API/families
    private String createLinkHrefFamilies(String apiUrl) {
        return createLinkHref(apiUrl, RestInternalConstants.LINK_SUBPATH_FAMILIES);
    }

    // API/families/family
    private String createLinkHrefFamily(String apiUrl, org.siemac.metamac.statistical.operations.core.domain.Family family) {
        String linkFamilies = createLinkHrefFamilies(apiUrl);
        return createLinkHref(linkFamilies, family.getCode());
    }

    // API/operations/OPERATION_ID/families
    private String createLinkHrefOperationChildrenFamilies(String apiUrl, org.siemac.metamac.statistical.operations.core.domain.Operation operation) {
        String linkFamily = createLinkHrefOperation(apiUrl, operation);
        return createLinkHref(linkFamily, RestInternalConstants.LINK_SUBPATH_FAMILIES);
    }

    // API/families/FAMILY_ID/operations
    private String createLinkHrefFamilyChildrenOperations(String apiUrl, org.siemac.metamac.statistical.operations.core.domain.Family family) {
        String linkFamily = createLinkHrefFamily(apiUrl, family);
        return createLinkHref(linkFamily, RestInternalConstants.LINK_SUBPATH_OPERATIONS);
    }

    private String createLinkHref(String baseLink, String additionalPath) {
        UriComponentsBuilder uriComponentsBuilder = UriComponentsBuilder.fromHttpUrl(baseLink);
        uriComponentsBuilder = uriComponentsBuilder.pathSegment(additionalPath);
        return uriComponentsBuilder.build().toUriString();
    }

    private org.siemac.metamac.statistical.operations.core.domain.Instance getInstanceInProcStatus(List<org.siemac.metamac.statistical.operations.core.domain.Instance> instances,
            ProcStatusEnum procStatus) {

        for (org.siemac.metamac.statistical.operations.core.domain.Instance instance : instances) {
            if (procStatus.equals(instance.getProcStatus())) {
                return instance;
            }
        }
        return null;
    }
}
