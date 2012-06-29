package org.siemac.metamac.statistical.operations.rest.internal.v1_0.mapper;

import java.io.Serializable;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Set;

import org.fornax.cartridges.sculptor.framework.domain.PagedResult;
import org.joda.time.DateTime;
import org.siemac.metamac.core.common.bt.domain.ExternalItemBt;
import org.siemac.metamac.core.common.exception.CommonServiceExceptionType;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.core.common.exception.MetamacExceptionItem;
import org.siemac.metamac.core.common.vo.domain.ExternalItem;
import org.siemac.metamac.rest.common.v1_0.domain.Error;
import org.siemac.metamac.rest.common.v1_0.domain.ErrorItem;
import org.siemac.metamac.rest.common.v1_0.domain.InternationalString;
import org.siemac.metamac.rest.common.v1_0.domain.LocalisedString;
import org.siemac.metamac.rest.common.v1_0.domain.RelatedResource;
import org.siemac.metamac.rest.search.PagedResultUtils;
import org.siemac.metamac.statistical.operations.core.domain.CollMethod;
import org.siemac.metamac.statistical.operations.core.domain.Cost;
import org.siemac.metamac.statistical.operations.core.domain.InstanceType;
import org.siemac.metamac.statistical.operations.core.domain.OfficialityType;
import org.siemac.metamac.statistical.operations.core.domain.SurveySource;
import org.siemac.metamac.statistical.operations.core.domain.SurveyType;
import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.rest.internal.RestInternalConstants;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.FamiliesNoPagedResult;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Family;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Instance;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.OperationsPagedResult;
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
        target.setSelfLink(toOperationLink(apiUrl, source));
        target.setTitle(toInternationalString(source.getTitle()));
        target.setAcronym(toInternationalString(source.getAcronym()));
        target.getFamilies().addAll(toRelatedResourcesFamilies(source.getFamilies(), apiUrl));
        target.setSubjectArea(toRelatedResource(source.getSubjectArea()));
        target.getSecondarySubjectAreas().addAll(toRelatedResourcesExternalItems(source.getSecondarySubjectAreas()));
        target.setObjective(toInternationalString(source.getObjective()));
        target.setDescription(toInternationalString(source.getDescription()));
        target.getInstances().addAll(toRelatedResourcesInstances(source.getInstances(), apiUrl));
        target.setSurveyType(toRelatedResource(source.getSurveyType()));
        target.setOfficialityType(toRelatedResource(source.getOfficialityType()));
        target.setIndicatorSystem(source.getIndicatorSystem());
        target.getProducers().addAll(toRelatedResourcesExternalItems(source.getProducer()));
        target.getRegionalResponsibles().addAll(toRelatedResourcesExternalItems(source.getRegionalResponsible()));
        target.getRegionalContributors().addAll(toRelatedResourcesExternalItems(source.getRegionalContributor()));
        target.setInternalInventoryDate(toDate(source.getInternalInventoryDate()));
        target.setCurrentlyActive(source.getCurrentlyActive());
        target.setStatus(source.getStatus() != null ? source.getStatus().name() : null);
        target.setProcStatus(source.getProcStatus().name());
        target.getPublishers().addAll(toRelatedResourcesExternalItems(source.getPublisher()));
        target.setRelPolUsAc(toInternationalString(source.getRelPolUsAc()));
        target.setReleaseCalendar(source.getReleaseCalendar());
        target.setReleaseCalendarAccess(source.getReleaseCalendarAccess());
        target.getUpdateFrequencies().addAll(toRelatedResourcesExternalItems(source.getUpdateFrequency()));
        target.setCurrentInstance(toRelatedResource(getInstanceInProcStatus(source.getInstances(), ProcStatusEnum.PUBLISH_EXTERNALLY), apiUrl));
        target.setCurrentInternalInstance(toRelatedResource(getInstanceInProcStatus(source.getInstances(), ProcStatusEnum.PUBLISH_INTERNALLY), apiUrl));
        target.setInventoryDate(toDate(source.getInventoryDate()));
        target.setRevPolicy(toInternationalString(source.getRevPolicy()));
        target.setRevPractice(toInternationalString(source.getRevPractice()));
        // TODO CONTACTS, LEGAL_ACTS, DATA_SHARING, CONFIDENTIALITY_POLICY, CONFIDENTIALITY_DATA_TREATMENT. No están en OperationBase
        target.setComment(toInternationalString(source.getComment()));
        target.setNotes(toInternationalString(source.getNotes()));
        target.setParent(toOperationParent(apiUrl));
        target.getchildren().addAll(toOperationChildren(source, apiUrl));
        return target;
    }

    @Override
    public OperationsPagedResult toOperationsByFamilyPagedResult(org.siemac.metamac.statistical.operations.core.domain.Family family,
            PagedResult<org.siemac.metamac.statistical.operations.core.domain.Operation> sourcesPagedResult, Integer limit, String apiUrl) {

        OperationsPagedResult targetPagedResult = new OperationsPagedResult();
        targetPagedResult.setKind(RestInternalConstants.KIND_OPERATIONS);

        // Values
        for (org.siemac.metamac.statistical.operations.core.domain.Operation source : sourcesPagedResult.getValues()) {
            RelatedResource target = toRelatedResource(source, apiUrl);
            targetPagedResult.getItems().add(target);
        }

        // Pagination
        // TODO pasar a librería común (transformPagedResult)
        targetPagedResult.setOffset(BigInteger.valueOf(sourcesPagedResult.getStartRow()));
        targetPagedResult.setLimit(BigInteger.valueOf(limit));
        targetPagedResult.setTotal(BigInteger.valueOf(sourcesPagedResult.getTotalRows()));
        // first page: only if it is not first page
        if (sourcesPagedResult.getStartRow() >= limit) {
            targetPagedResult.setFirstLink(toOperationsByFamilyLink(apiUrl, family, limit, 0));
        }
        // last page: only if it is not last page
        if (sourcesPagedResult.getStartRow() + limit < sourcesPagedResult.getTotalRows()) {
            targetPagedResult.setLastLink(toOperationsByFamilyLink(apiUrl, family, limit, PagedResultUtils.getOffsetLastPage(limit, sourcesPagedResult.getTotalRows())));
        }
        // previous and next page
        if (sourcesPagedResult.getRowCount() > 0) {
            int previousOffset = PagedResultUtils.getOffsetPreviousPage(limit, sourcesPagedResult.getStartRow());
            if (PagedResultUtils.NO_OFFSET != previousOffset) {
                targetPagedResult.setPreviousLink(toOperationsByFamilyLink(apiUrl, family, limit, previousOffset));
            }
            int nextOffset = PagedResultUtils.getOffsetNextPage(limit, sourcesPagedResult.getStartRow(), sourcesPagedResult.getTotalRows());
            if (PagedResultUtils.NO_OFFSET != nextOffset) {
                targetPagedResult.setNextLink(toOperationsByFamilyLink(apiUrl, family, limit, nextOffset));
            }
        }
        return targetPagedResult;
    }

    @Override
    public Family toFamily(org.siemac.metamac.statistical.operations.core.domain.Family source, String apiUrl) {
        if (source == null) {
            return null;
        }
        Family target = new Family();
        target.setId(source.getCode());
        target.setKind(RestInternalConstants.KIND_FAMILY);
        target.setSelfLink(toFamilyLink(apiUrl, source));
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
    public FamiliesNoPagedResult toFamiliesByOperationNoPagedResult(List<org.siemac.metamac.statistical.operations.core.domain.Family> sources, String apiUrl) {

        FamiliesNoPagedResult targets = new FamiliesNoPagedResult();
        targets.setKind(RestInternalConstants.KIND_FAMILIES);

        if (sources == null) {
            targets.setTotal(BigInteger.ZERO);
        } else {
            for (org.siemac.metamac.statistical.operations.core.domain.Family source : sources) {
                RelatedResource target = toRelatedResource(source, apiUrl);
                targets.getItems().add(target);
            }
            targets.setTotal(BigInteger.valueOf(sources.size()));
        }

        return targets;
    }

    @Override
    public Instance toInstance(org.siemac.metamac.statistical.operations.core.domain.Instance source, String apiUrl) {
        if (source == null) {
            return null;
        }
        Instance target = new Instance();
        target.setId(source.getCode());
        target.setKind(RestInternalConstants.KIND_INSTANCE);
        target.setSelfLink(toInstanceLink(apiUrl, source));
        target.setTitle(toInternationalString(source.getTitle()));
        target.setAcronym(toInternationalString(source.getAcronym()));
        target.setSurvey(toRelatedResource(source.getOperation(), apiUrl));
        target.setPredecessor(toRelatedResource(getInstanceInOrder(source.getOperation().getInstances(), source.getOrder() - 1), apiUrl));
        target.setSuccessor(toRelatedResource(getInstanceInOrder(source.getOperation().getInstances(), source.getOrder() + 1), apiUrl));
        target.setDataDescription(toInternationalString(source.getDataDescription()));
        target.setStatisticalPopulation(toInternationalString(source.getStatisticalPopulation()));
        target.getStatisticalUnits().addAll(toRelatedResourcesExternalItems(source.getStatisticalUnit()));
        target.setGeographicGranularity(toRelatedResource(source.getGeographicGranularity()));
        target.setGeographicComparability(toInternationalString(source.getGeographicComparability()));
        target.setTemporalGranularity(toRelatedResource(source.getTemporalGranularity()));
        target.setTemporalComparability(toInternationalString(source.getTemporalComparability()));
        target.setBasePeriod(source.getBasePeriod());
        target.getUnitMeasures().addAll(toRelatedResourcesExternalItems(source.getUnitMeasure()));
        target.setStatConcDef(toInternationalString(source.getStatConcDef()));
        target.getStatConcDefLists().addAll(toRelatedResourcesExternalItems(source.getStatConcDefList()));
        target.setClassSystem(toInternationalString(source.getClassSystem()));
        target.getClassSystemLists().addAll(toRelatedResourcesExternalItems(source.getClassSystemList()));
        target.setInstanceType(toRelatedResource(source.getInstanceType()));
        target.setInternalInventoryDate(toDate(source.getInternalInventoryDate()));
        target.setProcStatus(source.getProcStatus().name());
        target.setDocMethod(toInternationalString(source.getDocMethod()));
        target.setSurveySource(toRelatedResource(source.getSurveySource()));
        target.setCollMethod(toRelatedResource(source.getCollMethod()));
        target.getInformationSuppliers().addAll(toRelatedResourcesExternalItems(source.getInformationSuppliers()));
        target.getFreqColls().addAll(toRelatedResourcesExternalItems(source.getFreqColl()));
        target.setDataValidation(toInternationalString(source.getDataValidation()));
        target.setDataCompilation(toInternationalString(source.getDataCompilation()));
        target.setAdjustment(toInternationalString(source.getAdjustment()));
        target.setCostBurden(toInternationalString(source.getCostBurden()));
        target.getCosts().addAll(toRelatedResourcesCosts(source.getCost()));
        target.setInventoryDate(toDate(source.getInventoryDate()));
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

    private RelatedResource toRelatedResource(org.siemac.metamac.statistical.operations.core.domain.Operation source, String apiUrl) {
        if (source == null) {
            return null;
        }
        RelatedResource target = new RelatedResource();
        target.setId(source.getCode());
        target.setKind(RestInternalConstants.KIND_OPERATION);
        target.setSelfLink(toOperationLink(apiUrl, source));
        target.setTitle(toInternationalString(source.getTitle()));
        return target;
    }

    private List<RelatedResource> toRelatedResourcesFamilies(Set<org.siemac.metamac.statistical.operations.core.domain.Family> sources, String apiUrl) {
        List<RelatedResource> targets = new ArrayList<RelatedResource>();
        if (sources == null) {
            return targets;
        }
        for (org.siemac.metamac.statistical.operations.core.domain.Family source : sources) {
            RelatedResource target = toRelatedResource(source, apiUrl);
            targets.add(target);
        }
        return targets;
    }

    private RelatedResource toRelatedResource(org.siemac.metamac.statistical.operations.core.domain.Family source, String apiUrl) {
        if (source == null) {
            return null;
        }
        RelatedResource target = new RelatedResource();
        target.setId(source.getCode());
        target.setKind(RestInternalConstants.KIND_FAMILY);
        target.setSelfLink(toFamilyLink(apiUrl, source));
        target.setTitle(toInternationalString(source.getTitle()));
        return target;
    }

    private List<RelatedResource> toRelatedResourcesInstances(List<org.siemac.metamac.statistical.operations.core.domain.Instance> sources, String apiUrl) {
        List<RelatedResource> targets = new ArrayList<RelatedResource>();
        if (sources == null) {
            return targets;
        }
        for (org.siemac.metamac.statistical.operations.core.domain.Instance source : sources) {
            RelatedResource target = toRelatedResource(source, apiUrl);
            targets.add(target);
        }
        return targets;
    }

    private RelatedResource toRelatedResource(org.siemac.metamac.statistical.operations.core.domain.Instance source, String apiUrl) {
        if (source == null) {
            return null;
        }
        RelatedResource target = new RelatedResource();
        target.setId(source.getCode());
        target.setKind(RestInternalConstants.KIND_INSTANCE);
        target.setSelfLink(toInstanceLink(apiUrl, source));
        target.setTitle(toInternationalString(source.getTitle()));
        return target;
    }

    private RelatedResource toRelatedResource(SurveyType source) {
        if (source == null) {
            return null;
        }
        RelatedResource target = new RelatedResource();
        target.setId(source.getIdentifier());
        target.setTitle(toInternationalString(source.getDescription()));
        return target;
    }

    private RelatedResource toRelatedResource(OfficialityType source) {
        if (source == null) {
            return null;
        }
        RelatedResource target = new RelatedResource();
        target.setId(source.getIdentifier());
        target.setTitle(toInternationalString(source.getDescription()));
        return target;
    }

    private RelatedResource toRelatedResource(SurveySource source) {
        if (source == null) {
            return null;
        }
        RelatedResource target = new RelatedResource();
        target.setId(source.getIdentifier());
        target.setTitle(toInternationalString(source.getDescription()));
        return target;
    }

    private RelatedResource toRelatedResource(InstanceType source) {
        if (source == null) {
            return null;
        }
        RelatedResource target = new RelatedResource();
        target.setId(source.getIdentifier());
        target.setTitle(toInternationalString(source.getDescription()));
        return target;
    }

    private RelatedResource toRelatedResource(CollMethod source) {
        if (source == null) {
            return null;
        }
        RelatedResource target = new RelatedResource();
        target.setId(source.getIdentifier());
        target.setTitle(toInternationalString(source.getDescription()));
        return target;
    }

    private RelatedResource toRelatedResource(Cost source) {
        if (source == null) {
            return null;
        }
        RelatedResource target = new RelatedResource();
        target.setId(source.getIdentifier());
        target.setTitle(toInternationalString(source.getDescription()));
        return target;
    }

    private List<RelatedResource> toRelatedResourcesCosts(Set<Cost> sources) {
        List<RelatedResource> targets = new ArrayList<RelatedResource>();
        if (sources == null) {
            return targets;
        }
        for (Cost source : sources) {
            RelatedResource target = toRelatedResource(source);
            targets.add(target);
        }
        return targets;
    }

    private List<RelatedResource> toRelatedResourcesExternalItems(Set<ExternalItem> sources) {
        List<RelatedResource> targets = new ArrayList<RelatedResource>();
        if (sources == null) {
            return targets;
        }
        for (ExternalItem source : sources) {
            RelatedResource target = toRelatedResource(source.getExt());
            targets.add(target);
        }
        return targets;
    }

    private RelatedResource toRelatedResource(ExternalItemBt source) {
        if (source == null) {
            return null;
        }
        RelatedResource target = new RelatedResource();
        target.setId(source.getCodeId()); // TODO próximamente se cambiará por urn en ExternalItem
        target.setKind(source.getType().name());
        target.setSelfLink(source.getUriInt());
        target.setTitle(null); // TODO se añadirá Title a ExternalItem
        return target;
    }

    private RelatedResource toOperationParent(String apiUrl) {
        RelatedResource target = new RelatedResource();
        target.setKind(RestInternalConstants.KIND_OPERATIONS);
        target.setSelfLink(toOperationsLink(apiUrl));
        return target;
    }

    private List<RelatedResource> toOperationChildren(org.siemac.metamac.statistical.operations.core.domain.Operation operation, String apiUrl) {
        List<RelatedResource> targets = new ArrayList<RelatedResource>();

        // Instances
        RelatedResource instancesTarget = new RelatedResource();
        instancesTarget.setKind(RestInternalConstants.KIND_INSTANCES);
        instancesTarget.setSelfLink(toInstancesLink(apiUrl, operation));
        targets.add(instancesTarget);

        // Families
        RelatedResource familiesTarget = new RelatedResource();
        familiesTarget.setKind(RestInternalConstants.KIND_FAMILIES);
        familiesTarget.setSelfLink(toFamiliesByOperationLink(operation, apiUrl));
        targets.add(familiesTarget);

        return targets;
    }

    private RelatedResource toFamilyParent(String apiUrl) {
        RelatedResource target = new RelatedResource();
        target.setKind(RestInternalConstants.KIND_FAMILIES);
        target.setSelfLink(toFamiliesLink(apiUrl));
        return target;
    }

    private List<RelatedResource> toFamilyChildren(org.siemac.metamac.statistical.operations.core.domain.Family family, String apiUrl) {
        List<RelatedResource> targets = new ArrayList<RelatedResource>();

        // Operations of family
        RelatedResource operationsTarget = new RelatedResource();
        operationsTarget.setKind(RestInternalConstants.KIND_OPERATIONS);
        operationsTarget.setSelfLink(toOperationsByFamilyLink(family, apiUrl));
        targets.add(operationsTarget);

        return targets;
    }

    private RelatedResource toInstanceParent(org.siemac.metamac.statistical.operations.core.domain.Instance instance, String apiUrl) {
        RelatedResource target = new RelatedResource();
        target.setKind(RestInternalConstants.KIND_OPERATION);
        target.setSelfLink(toOperationLink(apiUrl, instance.getOperation()));
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

    // API/operations
    private String toOperationsLink(String apiUrl) {
        return createLink(apiUrl, RestInternalConstants.LINK_SUBPATH_OPERATIONS);
    }

    // API/operations/OPERATION_ID
    private String toOperationLink(String apiUrl, org.siemac.metamac.statistical.operations.core.domain.Operation operation) {
        String linkOperations = toOperationsLink(apiUrl);
        return createLink(linkOperations, operation.getCode());
    }

    // API/operations/OPERATION_ID/instances
    private String toInstancesLink(String apiUrl, org.siemac.metamac.statistical.operations.core.domain.Operation operation) {
        String linkOperation = toOperationLink(apiUrl, operation);
        return createLink(linkOperation, RestInternalConstants.LINK_SUBPATH_INSTANCES);
    }

    // API/operations/OPERATION_ID/instances/INSTANCE_ID
    private String toInstanceLink(String apiUrl, org.siemac.metamac.statistical.operations.core.domain.Instance instance) {
        String linkOperation = toInstancesLink(apiUrl, instance.getOperation());
        return createLink(linkOperation, instance.getCode());
    }

    // API/families
    private String toFamiliesLink(String apiUrl) {
        return createLink(apiUrl, RestInternalConstants.LINK_SUBPATH_FAMILIES);
    }

    // API/families/family
    private String toFamilyLink(String apiUrl, org.siemac.metamac.statistical.operations.core.domain.Family family) {
        String linkFamilies = toFamiliesLink(apiUrl);
        return createLink(linkFamilies, family.getCode());
    }

    // API/operations/OPERATION_ID/families
    private String toFamiliesByOperationLink(org.siemac.metamac.statistical.operations.core.domain.Operation operation, String apiUrl) {
        String linkFamily = toOperationLink(apiUrl, operation);
        return createLink(linkFamily, RestInternalConstants.LINK_SUBPATH_FAMILIES);
    }

    // API/families/FAMILY_ID/operations
    private String toOperationsByFamilyLink(org.siemac.metamac.statistical.operations.core.domain.Family family, String apiUrl) {
        String linkFamily = toFamilyLink(apiUrl, family);
        return createLink(linkFamily, RestInternalConstants.LINK_SUBPATH_OPERATIONS);
    }

    // API/families/FAMILY_ID/operations?limit=?&offset=?
    private String toOperationsByFamilyLink(String apiUrl, org.siemac.metamac.statistical.operations.core.domain.Family family, int limit, int offset) {
        if (offset < 0) {
            return null;
        }
        String linkFamily = toFamilyLink(apiUrl, family);
        String linkFamilyOperations = createLink(linkFamily, RestInternalConstants.LINK_SUBPATH_OPERATIONS);
        linkFamilyOperations = createLinkWithQueryParam(linkFamilyOperations, RestInternalConstants.QUERY_PARAM_LIMIT, String.valueOf(limit));
        linkFamilyOperations = createLinkWithQueryParam(linkFamilyOperations, RestInternalConstants.QUERY_PARAM_OFFSET, String.valueOf(offset));
        return linkFamilyOperations;
    }

    private String createLink(String baseLink, String additionalPath) {
        UriComponentsBuilder uriComponentsBuilder = UriComponentsBuilder.fromHttpUrl(baseLink);
        uriComponentsBuilder = uriComponentsBuilder.pathSegment(additionalPath);
        return uriComponentsBuilder.build().toUriString();
    }

    private String createLinkWithQueryParam(String baseLink, String queryParam, String queryParamValue) {
        UriComponentsBuilder uriComponentsBuilder = UriComponentsBuilder.fromHttpUrl(baseLink);
        uriComponentsBuilder = uriComponentsBuilder.queryParam(queryParam, queryParamValue);
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

    private org.siemac.metamac.statistical.operations.core.domain.Instance getInstanceInOrder(List<org.siemac.metamac.statistical.operations.core.domain.Instance> instances, Integer order) {

        for (org.siemac.metamac.statistical.operations.core.domain.Instance instance : instances) {
            if (order.equals(instance.getOrder())) {
                if (ProcStatusEnum.PUBLISH_INTERNALLY.equals(instance.getProcStatus()) || ProcStatusEnum.PUBLISH_EXTERNALLY.equals(instance.getProcStatus())) {
                    return instance;
                } else {
                    return null;
                }
            }
        }
        return null;
    }
}
