package org.siemac.metamac.statistical.operations.rest.internal.v1_0.mapper;

import java.io.Serializable;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Set;

import javax.ws.rs.core.Response.Status;

import org.fornax.cartridges.sculptor.framework.domain.PagedResult;
import org.joda.time.DateTime;
import org.siemac.metamac.core.common.ent.domain.ExternalItem;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.core.common.exception.MetamacExceptionItem;
import org.siemac.metamac.rest.common.v1_0.domain.Error;
import org.siemac.metamac.rest.common.v1_0.domain.ErrorItem;
import org.siemac.metamac.rest.common.v1_0.domain.InternationalString;
import org.siemac.metamac.rest.common.v1_0.domain.LocalisedString;
import org.siemac.metamac.rest.common.v1_0.domain.Resource;
import org.siemac.metamac.rest.common.v1_0.domain.ResourcesNoPagedResult;
import org.siemac.metamac.rest.common.v1_0.domain.ResourcesPagedResult;
import org.siemac.metamac.rest.exception.RestCommonServiceExceptionType;
import org.siemac.metamac.rest.exception.RestException;
import org.siemac.metamac.rest.exception.utils.RestExceptionUtils;
import org.siemac.metamac.rest.search.criteria.mapper.SculptorCriteria2RestCriteria;
import org.siemac.metamac.rest.utils.RestUtils;
import org.siemac.metamac.statistical.operations.core.domain.CollMethod;
import org.siemac.metamac.statistical.operations.core.domain.Cost;
import org.siemac.metamac.statistical.operations.core.domain.InstanceType;
import org.siemac.metamac.statistical.operations.core.domain.OfficialityType;
import org.siemac.metamac.statistical.operations.core.domain.SurveySource;
import org.siemac.metamac.statistical.operations.core.domain.SurveyType;
import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.rest.internal.RestInternalConstants;
import org.siemac.metamac.statistical.operations.rest.internal.exception.RestServiceExceptionType;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Family;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Instance;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.ProcStatus;
import org.springframework.stereotype.Component;

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
        target.setUrn(source.getUrn());
        target.setKind(RestInternalConstants.KIND_OPERATION);
        target.setSelfLink(toOperationLink(apiUrl, source));
        target.setTitle(toInternationalString(source.getTitle()));
        target.setAcronym(toInternationalString(source.getAcronym()));
        target.getFamilies().addAll(toResourcesFamilies(source.getFamilies(), apiUrl));
        target.setSubjectArea(toResource(source.getSubjectArea()));
        target.getSecondarySubjectAreas().addAll(toResourcesExternalItems(source.getSecondarySubjectAreas()));
        target.setObjective(toInternationalString(source.getObjective()));
        target.setDescription(toInternationalString(source.getDescription()));
        target.getInstances().addAll(toResourcesInstances(source.getInstances(), apiUrl));
        target.setSurveyType(toResource(source.getSurveyType()));
        target.setOfficialityType(toResource(source.getOfficialityType()));
        target.setIndicatorSystem(source.getIndicatorSystem());
        target.getProducers().addAll(toResourcesExternalItems(source.getProducer()));
        target.getRegionalResponsibles().addAll(toResourcesExternalItems(source.getRegionalResponsible()));
        target.getRegionalContributors().addAll(toResourcesExternalItems(source.getRegionalContributor()));
        target.setInternalInventoryDate(toDate(source.getInternalInventoryDate()));
        target.setCurrentlyActive(source.getCurrentlyActive());
        target.setStatus(source.getStatus() != null ? source.getStatus().name() : null);
        target.setProcStatus(toProcStatus(source.getProcStatus()));
        target.getPublishers().addAll(toResourcesExternalItems(source.getPublisher()));
        target.setRelPolUsAc(toInternationalString(source.getRelPolUsAc()));
        target.setReleaseCalendar(source.getReleaseCalendar());
        target.setReleaseCalendarAccess(source.getReleaseCalendarAccess());
        target.getUpdateFrequencies().addAll(toResourcesExternalItems(source.getUpdateFrequency()));
        target.setCurrentInstance(toResource(getInstanceInProcStatus(source.getInstances(), ProcStatusEnum.PUBLISH_EXTERNALLY), apiUrl));
        target.setCurrentInternalInstance(toResource(getInstanceInProcStatus(source.getInstances(), ProcStatusEnum.PUBLISH_INTERNALLY), apiUrl));
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
    public ResourcesPagedResult toOperationsPagedResult(PagedResult<org.siemac.metamac.statistical.operations.core.domain.Operation> sourcesPagedResult, String query, String orderBy, Integer limit,
            String apiUrl) {

        ResourcesPagedResult targetPagedResult = new ResourcesPagedResult();
        targetPagedResult.setKind(RestInternalConstants.KIND_OPERATIONS);

        // Pagination
        String baseLink = toOperationsLink(apiUrl);
        SculptorCriteria2RestCriteria.toPagedResult(sourcesPagedResult, targetPagedResult, query, orderBy, limit, baseLink);

        // Values
        for (org.siemac.metamac.statistical.operations.core.domain.Operation source : sourcesPagedResult.getValues()) {
            Resource target = toResource(source, apiUrl);
            targetPagedResult.getItems().add(target);
        }
        return targetPagedResult;
    }

    @Override
    public ResourcesPagedResult toOperationsByFamilyPagedResult(org.siemac.metamac.statistical.operations.core.domain.Family family,
            PagedResult<org.siemac.metamac.statistical.operations.core.domain.Operation> sourcesPagedResult, String query, String orderBy, Integer limit, String apiUrl) {

        ResourcesPagedResult targetPagedResult = new ResourcesPagedResult();
        targetPagedResult.setKind(RestInternalConstants.KIND_OPERATIONS);

        // Pagination
        String baseLink = toOperationsByFamilyLink(family, apiUrl);
        SculptorCriteria2RestCriteria.toPagedResult(sourcesPagedResult, targetPagedResult, query, orderBy, limit, baseLink);

        // Values
        for (org.siemac.metamac.statistical.operations.core.domain.Operation source : sourcesPagedResult.getValues()) {
            Resource target = toResource(source, apiUrl);
            targetPagedResult.getItems().add(target);
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
        target.setUrn(source.getUrn());
        target.setKind(RestInternalConstants.KIND_FAMILY);
        target.setSelfLink(toFamilyLink(apiUrl, source));
        target.setTitle(toInternationalString(source.getTitle()));
        target.setAcronym(toInternationalString(source.getAcronym()));
        target.setDescription(toInternationalString(source.getDescription()));
        target.setInternalInventoryDate(toDate(source.getInternalInventoryDate()));
        target.setProcStatus(toProcStatus(source.getProcStatus()));
        target.setInventoryDate(toDate(source.getInventoryDate()));
        target.setParent(toFamilyParent(apiUrl));
        target.getchildren().addAll(toFamilyChildren(source, apiUrl));
        return target;
    }

    @Override
    public ResourcesPagedResult toFamiliesPagedResult(PagedResult<org.siemac.metamac.statistical.operations.core.domain.Family> sourcesPagedResult, String query, String orderBy, Integer limit,
            String apiUrl) {

        ResourcesPagedResult targetPagedResult = new ResourcesPagedResult();
        targetPagedResult.setKind(RestInternalConstants.KIND_FAMILIES);

        // Pagination
        String baseLink = toFamiliesLink(apiUrl);
        SculptorCriteria2RestCriteria.toPagedResult(sourcesPagedResult, targetPagedResult, query, orderBy, limit, baseLink);

        // Values
        for (org.siemac.metamac.statistical.operations.core.domain.Family source : sourcesPagedResult.getValues()) {
            Resource target = toResource(source, apiUrl);
            targetPagedResult.getItems().add(target);
        }
        return targetPagedResult;
    }

    @Override
    public ResourcesNoPagedResult toFamiliesByOperationNoPagedResult(List<org.siemac.metamac.statistical.operations.core.domain.Family> sources, String apiUrl) {

        ResourcesNoPagedResult targets = new ResourcesNoPagedResult();
        targets.setKind(RestInternalConstants.KIND_FAMILIES);

        if (sources == null) {
            targets.setTotal(BigInteger.ZERO);
        } else {
            for (org.siemac.metamac.statistical.operations.core.domain.Family source : sources) {
                Resource target = toResource(source, apiUrl);
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
        target.setUrn(source.getUrn());
        target.setKind(RestInternalConstants.KIND_INSTANCE);
        target.setSelfLink(toInstanceLink(apiUrl, source));
        target.setTitle(toInternationalString(source.getTitle()));
        target.setAcronym(toInternationalString(source.getAcronym()));
        target.setSurvey(toResource(source.getOperation(), apiUrl));
        target.setPredecessor(toResource(getInstanceInOrder(source.getOperation().getInstances(), source.getOrder() - 1), apiUrl));
        target.setSuccessor(toResource(getInstanceInOrder(source.getOperation().getInstances(), source.getOrder() + 1), apiUrl));
        target.setDataDescription(toInternationalString(source.getDataDescription()));
        target.setStatisticalPopulation(toInternationalString(source.getStatisticalPopulation()));
        target.getStatisticalUnits().addAll(toResourcesExternalItems(source.getStatisticalUnit()));
        target.setGeographicGranularity(toResource(source.getGeographicGranularity()));
        target.setGeographicComparability(toInternationalString(source.getGeographicComparability()));
        target.setTemporalGranularity(toResource(source.getTemporalGranularity()));
        target.setTemporalComparability(toInternationalString(source.getTemporalComparability()));
        target.setBasePeriod(source.getBasePeriod());
        target.getUnitMeasures().addAll(toResourcesExternalItems(source.getUnitMeasure()));
        target.setStatConcDef(toInternationalString(source.getStatConcDef()));
        target.getStatConcDefLists().addAll(toResourcesExternalItems(source.getStatConcDefList()));
        target.setClassSystem(toInternationalString(source.getClassSystem()));
        target.getClassSystemLists().addAll(toResourcesExternalItems(source.getClassSystemList()));
        target.setInstanceType(toResource(source.getInstanceType()));
        target.setInternalInventoryDate(toDate(source.getInternalInventoryDate()));
        target.setProcStatus(toProcStatus(source.getProcStatus()));
        target.setDocMethod(toInternationalString(source.getDocMethod()));
        target.setSurveySource(toResource(source.getSurveySource()));
        target.setCollMethod(toResource(source.getCollMethod()));
        target.getInformationSuppliers().addAll(toResourcesExternalItems(source.getInformationSuppliers()));
        target.getFreqColls().addAll(toResourcesExternalItems(source.getFreqColl()));
        target.setDataValidation(toInternationalString(source.getDataValidation()));
        target.setDataCompilation(toInternationalString(source.getDataCompilation()));
        target.setAdjustment(toInternationalString(source.getAdjustment()));
        target.setCostBurden(toInternationalString(source.getCostBurden()));
        target.getCosts().addAll(toResourcesCosts(source.getCost()));
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

    @Override
    public ResourcesPagedResult toInstancesPagedResult(org.siemac.metamac.statistical.operations.core.domain.Operation operation,
            PagedResult<org.siemac.metamac.statistical.operations.core.domain.Instance> sourcesPagedResult, String query, String orderBy, Integer limit, String apiUrl) {

        ResourcesPagedResult targetPagedResult = new ResourcesPagedResult();
        targetPagedResult.setKind(RestInternalConstants.KIND_INSTANCES);

        // Pagination
        String baseLink = toInstancesLink(apiUrl, operation);
        SculptorCriteria2RestCriteria.toPagedResult(sourcesPagedResult, targetPagedResult, query, orderBy, limit, baseLink);

        // Values
        for (org.siemac.metamac.statistical.operations.core.domain.Instance source : sourcesPagedResult.getValues()) {
            Resource target = toResource(source, apiUrl);
            targetPagedResult.getItems().add(target);
        }
        return targetPagedResult;
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
            ErrorItem errorItem = RestExceptionUtils.getErrorItem(RestCommonServiceExceptionType.UNKNOWN);
            errorItems.add(errorItem);
        }
        return errorItems;
    }

    private Resource toResource(org.siemac.metamac.statistical.operations.core.domain.Operation source, String apiUrl) {
        if (source == null) {
            return null;
        }
        Resource target = new Resource();
        target.setId(source.getCode());
        target.setKind(RestInternalConstants.KIND_OPERATION);
        target.setSelfLink(toOperationLink(apiUrl, source));
        target.setTitle(toInternationalString(source.getTitle()));
        return target;
    }

    private List<Resource> toResourcesFamilies(Set<org.siemac.metamac.statistical.operations.core.domain.Family> sources, String apiUrl) {
        List<Resource> targets = new ArrayList<Resource>();
        if (sources == null) {
            return targets;
        }
        for (org.siemac.metamac.statistical.operations.core.domain.Family source : sources) {
            Resource target = toResource(source, apiUrl);
            targets.add(target);
        }
        return targets;
    }

    private Resource toResource(org.siemac.metamac.statistical.operations.core.domain.Family source, String apiUrl) {
        if (source == null) {
            return null;
        }
        Resource target = new Resource();
        target.setId(source.getCode());
        target.setKind(RestInternalConstants.KIND_FAMILY);
        target.setSelfLink(toFamilyLink(apiUrl, source));
        target.setTitle(toInternationalString(source.getTitle()));
        return target;
    }

    private List<Resource> toResourcesInstances(List<org.siemac.metamac.statistical.operations.core.domain.Instance> sources, String apiUrl) {
        List<Resource> targets = new ArrayList<Resource>();
        if (sources == null) {
            return targets;
        }
        for (org.siemac.metamac.statistical.operations.core.domain.Instance source : sources) {
            Resource target = toResource(source, apiUrl);
            targets.add(target);
        }
        return targets;
    }

    private Resource toResource(org.siemac.metamac.statistical.operations.core.domain.Instance source, String apiUrl) {
        if (source == null) {
            return null;
        }
        Resource target = new Resource();
        target.setId(source.getCode());
        target.setKind(RestInternalConstants.KIND_INSTANCE);
        target.setSelfLink(toInstanceLink(apiUrl, source));
        target.setTitle(toInternationalString(source.getTitle()));
        return target;
    }

    private Resource toResource(SurveyType source) {
        if (source == null) {
            return null;
        }
        Resource target = new Resource();
        target.setId(source.getIdentifier());
        target.setTitle(toInternationalString(source.getDescription()));
        return target;
    }

    private Resource toResource(OfficialityType source) {
        if (source == null) {
            return null;
        }
        Resource target = new Resource();
        target.setId(source.getIdentifier());
        target.setTitle(toInternationalString(source.getDescription()));
        return target;
    }

    private Resource toResource(SurveySource source) {
        if (source == null) {
            return null;
        }
        Resource target = new Resource();
        target.setId(source.getIdentifier());
        target.setTitle(toInternationalString(source.getDescription()));
        return target;
    }

    private Resource toResource(InstanceType source) {
        if (source == null) {
            return null;
        }
        Resource target = new Resource();
        target.setId(source.getIdentifier());
        target.setTitle(toInternationalString(source.getDescription()));
        return target;
    }

    private Resource toResource(CollMethod source) {
        if (source == null) {
            return null;
        }
        Resource target = new Resource();
        target.setId(source.getIdentifier());
        target.setTitle(toInternationalString(source.getDescription()));
        return target;
    }

    private Resource toResource(Cost source) {
        if (source == null) {
            return null;
        }
        Resource target = new Resource();
        target.setId(source.getIdentifier());
        target.setTitle(toInternationalString(source.getDescription()));
        return target;
    }

    private List<Resource> toResourcesCosts(Set<Cost> sources) {
        List<Resource> targets = new ArrayList<Resource>();
        if (sources == null) {
            return targets;
        }
        for (Cost source : sources) {
            Resource target = toResource(source);
            targets.add(target);
        }
        return targets;
    }

    private List<Resource> toResourcesExternalItems(Set<ExternalItem> sources) {
        List<Resource> targets = new ArrayList<Resource>();
        if (sources == null) {
            return targets;
        }
        for (ExternalItem source : sources) {
            Resource target = toResource(source);
            targets.add(target);
        }
        return targets;
    }

    private Resource toResource(ExternalItem source) {
        if (source == null) {
            return null;
        }
        // TODO 
//      target.setId(source.getCode());
//      target.setUrn(source.getUrn());
        Resource target = new Resource();
        target.setId(source.getUrn());		
        target.setKind(source.getType().name());
        target.setSelfLink(source.getUri()); // TODO añadir endpoint METAMAC-785
        target.setTitle(toInternationalString(source.getTitle()));
        return target;
    }

    private Resource toOperationParent(String apiUrl) {
        Resource target = new Resource();
        target.setKind(RestInternalConstants.KIND_OPERATIONS);
        target.setSelfLink(toOperationsLink(apiUrl));
        return target;
    }

    private List<Resource> toOperationChildren(org.siemac.metamac.statistical.operations.core.domain.Operation operation, String apiUrl) {
        List<Resource> targets = new ArrayList<Resource>();

        // Instances
        Resource instancesTarget = new Resource();
        instancesTarget.setKind(RestInternalConstants.KIND_INSTANCES);
        instancesTarget.setSelfLink(toInstancesLink(apiUrl, operation));
        targets.add(instancesTarget);

        // Families
        Resource familiesTarget = new Resource();
        familiesTarget.setKind(RestInternalConstants.KIND_FAMILIES);
        familiesTarget.setSelfLink(toFamiliesByOperationLink(operation, apiUrl));
        targets.add(familiesTarget);

        return targets;
    }

    private Resource toFamilyParent(String apiUrl) {
        Resource target = new Resource();
        target.setKind(RestInternalConstants.KIND_FAMILIES);
        target.setSelfLink(toFamiliesLink(apiUrl));
        return target;
    }

    private List<Resource> toFamilyChildren(org.siemac.metamac.statistical.operations.core.domain.Family family, String apiUrl) {
        List<Resource> targets = new ArrayList<Resource>();

        // Operations of family
        Resource operationsTarget = new Resource();
        operationsTarget.setKind(RestInternalConstants.KIND_OPERATIONS);
        operationsTarget.setSelfLink(toOperationsByFamilyLink(family, apiUrl));
        targets.add(operationsTarget);

        return targets;
    }

    private Resource toInstanceParent(org.siemac.metamac.statistical.operations.core.domain.Instance instance, String apiUrl) {
        Resource target = new Resource();
        target.setKind(RestInternalConstants.KIND_OPERATION);
        target.setSelfLink(toOperationLink(apiUrl, instance.getOperation()));
        return target;
    }

    private List<Resource> toInstanceChildren(org.siemac.metamac.statistical.operations.core.domain.Instance instance, String apiUrl) {
        List<Resource> targets = new ArrayList<Resource>();
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
        return RestUtils.createLink(apiUrl, RestInternalConstants.LINK_SUBPATH_OPERATIONS);
    }

    // API/operations/OPERATION_ID
    private String toOperationLink(String apiUrl, org.siemac.metamac.statistical.operations.core.domain.Operation operation) {
        String linkOperations = toOperationsLink(apiUrl);
        return RestUtils.createLink(linkOperations, operation.getCode());
    }

    // API/operations/OPERATION_ID/instances
    private String toInstancesLink(String apiUrl, org.siemac.metamac.statistical.operations.core.domain.Operation operation) {
        String linkOperation = toOperationLink(apiUrl, operation);
        return RestUtils.createLink(linkOperation, RestInternalConstants.LINK_SUBPATH_INSTANCES);
    }

    // API/operations/OPERATION_ID/instances/INSTANCE_ID
    private String toInstanceLink(String apiUrl, org.siemac.metamac.statistical.operations.core.domain.Instance instance) {
        String linkOperation = toInstancesLink(apiUrl, instance.getOperation());
        return RestUtils.createLink(linkOperation, instance.getCode());
    }

    // API/families
    private String toFamiliesLink(String apiUrl) {
        return RestUtils.createLink(apiUrl, RestInternalConstants.LINK_SUBPATH_FAMILIES);
    }

    // API/families/family
    private String toFamilyLink(String apiUrl, org.siemac.metamac.statistical.operations.core.domain.Family family) {
        String linkFamilies = toFamiliesLink(apiUrl);
        return RestUtils.createLink(linkFamilies, family.getCode());
    }

    // API/operations/OPERATION_ID/families
    private String toFamiliesByOperationLink(org.siemac.metamac.statistical.operations.core.domain.Operation operation, String apiUrl) {
        String linkFamily = toOperationLink(apiUrl, operation);
        return RestUtils.createLink(linkFamily, RestInternalConstants.LINK_SUBPATH_FAMILIES);
    }

    // API/families/FAMILY_ID/operations
    private String toOperationsByFamilyLink(org.siemac.metamac.statistical.operations.core.domain.Family family, String apiUrl) {
        String linkFamily = toFamilyLink(apiUrl, family);
        return RestUtils.createLink(linkFamily, RestInternalConstants.LINK_SUBPATH_OPERATIONS);
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
    
    private ProcStatus toProcStatus(ProcStatusEnum source) {
        if (source == null) {
            return null;
        }
        switch (source) {
            case PUBLISH_INTERNALLY:
                return ProcStatus.PUBLISH_INTERNALLY;
            case PUBLISH_EXTERNALLY:
                return ProcStatus.PUBLISH_EXTERNALLY;
            default:
                Error error = RestExceptionUtils.getError(RestServiceExceptionType.UNKNOWN);
                throw new RestException(error, Status.INTERNAL_SERVER_ERROR);
        }
    }

}
