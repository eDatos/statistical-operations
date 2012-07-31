package org.siemac.metamac.statistical.operations.rest.internal.v1_0.mapper;

import java.io.Serializable;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Set;

import javax.ws.rs.core.Response.Status;

import org.apache.commons.lang.StringUtils;
import org.fornax.cartridges.sculptor.framework.domain.PagedResult;
import org.joda.time.DateTime;
import org.siemac.metamac.common.metadata.rest.internal.v1_0.domain.Configuration;
import org.siemac.metamac.core.common.conf.ConfigurationService;
import org.siemac.metamac.core.common.ent.domain.ExternalItem;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.core.common.exception.MetamacExceptionItem;
import org.siemac.metamac.rest.common.v1_0.domain.Error;
import org.siemac.metamac.rest.common.v1_0.domain.ErrorItem;
import org.siemac.metamac.rest.common.v1_0.domain.InternationalString;
import org.siemac.metamac.rest.common.v1_0.domain.LocalisedString;
import org.siemac.metamac.rest.common.v1_0.domain.Resource;
import org.siemac.metamac.rest.common.v1_0.domain.ResourceLink;
import org.siemac.metamac.rest.common.v1_0.domain.ResourcesNoPagedResult;
import org.siemac.metamac.rest.common.v1_0.domain.ResourcesPagedResult;
import org.siemac.metamac.rest.common.v1_0.domain.SimpleItem;
import org.siemac.metamac.rest.common.v1_0.domain.SimpleItemsNoPagedResult;
import org.siemac.metamac.rest.constants.RestEndpointsConstants;
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
import org.siemac.metamac.statistical.operations.core.enume.domain.StatusEnum;
import org.siemac.metamac.statistical.operations.rest.internal.RestInternalConstants;
import org.siemac.metamac.statistical.operations.rest.internal.exception.RestServiceExceptionType;
import org.siemac.metamac.statistical.operations.rest.internal.invocation.CommonMetadataRestInternalFacade;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Family;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Instance;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.ProcStatus;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class Do2RestInternalMapperV10Impl implements Do2RestInternalMapperV10 {

    // @Context
    // private MessageContext context; // Always null in this bean (not in Service)...

    @Autowired
    private ConfigurationService             configurationService;

    @Autowired
    private CommonMetadataRestInternalFacade commonMetadataRestInternalFacade;

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
        target.setSubjectArea(toResourceExternalItemSrm(source.getSubjectArea()));
        target.getSecondarySubjectAreas().addAll(toResourcesExternalItemsSrm(source.getSecondarySubjectAreas()));
        target.setObjective(toInternationalString(source.getObjective()));
        target.setDescription(toInternationalString(source.getDescription()));
        target.getInstances().addAll(toResourcesInstances(source.getInstances(), apiUrl));
        target.setSurveyType(toSimpleItem(source.getSurveyType()));
        target.setOfficialityType(toSimpleItem(source.getOfficialityType()));
        target.setIndicatorSystem(source.getIndicatorSystem());
        target.getProducers().addAll(toResourcesExternalItemsSrm(source.getProducer()));
        target.getRegionalResponsibles().addAll(toResourcesExternalItemsSrm(source.getRegionalResponsible()));
        target.getRegionalContributors().addAll(toResourcesExternalItemsSrm(source.getRegionalContributor()));
        target.setInternalInventoryDate(toDate(source.getInternalInventoryDate()));
        target.setCurrentlyActive(source.getCurrentlyActive());
        target.setStatus(toStatus(source.getStatus()));
        target.setProcStatus(toProcStatus(source.getProcStatus()));
        target.getPublishers().addAll(toResourcesExternalItemsSrm(source.getPublisher()));
        target.setRelPolUsAc(toInternationalString(source.getRelPolUsAc()));
        target.setReleaseCalendar(source.getReleaseCalendar());
        target.setReleaseCalendarAccess(source.getReleaseCalendarAccess());
        target.getUpdateFrequencies().addAll(toResourcesExternalItemsSrm(source.getUpdateFrequency()));
        target.setCurrentInstance(toResource(getInstanceInProcStatus(source.getInstances(), ProcStatusEnum.PUBLISH_EXTERNALLY), apiUrl));
        target.setCurrentInternalInstance(toResource(getInstanceInProcStatus(source.getInstances(), ProcStatusEnum.PUBLISH_INTERNALLY), apiUrl));
        target.setInventoryDate(toDate(source.getInventoryDate()));
        target.setRevPolicy(toInternationalString(source.getRevPolicy()));
        target.setRevPractice(toInternationalString(source.getRevPractice()));
        commonMetadataToOperation(source.getCommonMetadata(), target);
        target.setComment(toInternationalString(source.getComment()));
        target.setNotes(toInternationalString(source.getNotes()));
        target.setParent(toOperationParent(apiUrl));
        target.getChildren().addAll(toOperationChildren(source, apiUrl));
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
        target.getChildren().addAll(toFamilyChildren(source, apiUrl));
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
        target.getStatisticalUnits().addAll(toResourcesExternalItemsSrm(source.getStatisticalUnit()));
        target.setGeographicGranularity(toResourceExternalItemSrm(source.getGeographicGranularity()));
        target.setGeographicComparability(toInternationalString(source.getGeographicComparability()));
        target.setTemporalGranularity(toResourceExternalItemSrm(source.getTemporalGranularity()));
        target.setTemporalComparability(toInternationalString(source.getTemporalComparability()));
        target.setBasePeriod(source.getBasePeriod());
        target.getUnitMeasures().addAll(toResourcesExternalItemsSrm(source.getUnitMeasure()));
        target.setStatConcDef(toInternationalString(source.getStatConcDef()));
        target.getStatConcDefLists().addAll(toResourcesExternalItemsSrm(source.getStatConcDefList()));
        target.setClassSystem(toInternationalString(source.getClassSystem()));
        target.getClassSystemLists().addAll(toResourcesExternalItemsSrm(source.getClassSystemList()));
        target.setInstanceType(toSimpleItem(source.getInstanceType()));
        target.setInternalInventoryDate(toDate(source.getInternalInventoryDate()));
        target.setProcStatus(toProcStatus(source.getProcStatus()));
        target.setDocMethod(toInternationalString(source.getDocMethod()));
        target.setSurveySource(toSimpleItem(source.getSurveySource()));
        target.setCollMethod(toSimpleItem(source.getCollMethod()));
        target.getInformationSuppliers().addAll(toResourcesExternalItemsSrm(source.getInformationSuppliers()));
        target.getFreqColls().addAll(toResourcesExternalItemsSrm(source.getFreqColl()));
        target.setDataValidation(toInternationalString(source.getDataValidation()));
        target.setDataCompilation(toInternationalString(source.getDataCompilation()));
        target.setAdjustment(toInternationalString(source.getAdjustment()));
        target.setCostBurden(toInternationalString(source.getCostBurden()));
        target.getCosts().addAll(toSimpleItemsCosts(source.getCost()));
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
        target.getChildren().addAll(toInstanceChildren(source, apiUrl));
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

    @Override
    public SimpleItemsNoPagedResult toSurveyTypesNoPagedResult(List<SurveyType> sources, String apiUrl) {
        SimpleItemsNoPagedResult targets = new SimpleItemsNoPagedResult();
        targets.setKind(RestInternalConstants.KIND_SURVEY_TYPES);

        if (sources == null) {
            targets.setTotal(BigInteger.ZERO);
        } else {
            for (org.siemac.metamac.statistical.operations.core.domain.SurveyType source : sources) {
                SimpleItem target = toSimpleItem(source);
                targets.getItems().add(target);
            }
            targets.setTotal(BigInteger.valueOf(sources.size()));
        }

        return targets;
    }

    @Override
    public SimpleItemsNoPagedResult toOfficialityTypesNoPagedResult(List<OfficialityType> sources, String apiUrl) {
        SimpleItemsNoPagedResult targets = new SimpleItemsNoPagedResult();
        targets.setKind(RestInternalConstants.KIND_OFFICIALITY_TYPES);

        if (sources == null) {
            targets.setTotal(BigInteger.ZERO);
        } else {
            for (org.siemac.metamac.statistical.operations.core.domain.OfficialityType source : sources) {
                SimpleItem target = toSimpleItem(source);
                targets.getItems().add(target);
            }
            targets.setTotal(BigInteger.valueOf(sources.size()));
        }

        return targets;
    }

    @Override
    public SimpleItemsNoPagedResult toInstanceTypesNoPagedResult(List<InstanceType> sources, String apiUrl) {
        SimpleItemsNoPagedResult targets = new SimpleItemsNoPagedResult();
        targets.setKind(RestInternalConstants.KIND_INSTANCE_TYPES);

        if (sources == null) {
            targets.setTotal(BigInteger.ZERO);
        } else {
            for (org.siemac.metamac.statistical.operations.core.domain.InstanceType source : sources) {
                SimpleItem target = toSimpleItem(source);
                targets.getItems().add(target);
            }
            targets.setTotal(BigInteger.valueOf(sources.size()));
        }

        return targets;
    }

    @Override
    public SimpleItemsNoPagedResult toSurveySourcesNoPagedResult(List<SurveySource> sources, String apiUrl) {
        SimpleItemsNoPagedResult targets = new SimpleItemsNoPagedResult();
        targets.setKind(RestInternalConstants.KIND_SURVEY_SOURCES);

        if (sources == null) {
            targets.setTotal(BigInteger.ZERO);
        } else {
            for (org.siemac.metamac.statistical.operations.core.domain.SurveySource source : sources) {
                SimpleItem target = toSimpleItem(source);
                targets.getItems().add(target);
            }
            targets.setTotal(BigInteger.valueOf(sources.size()));
        }

        return targets;
    }

    @Override
    public SimpleItemsNoPagedResult toCollMethodsNoPagedResult(List<CollMethod> sources, String apiUrl) {
        SimpleItemsNoPagedResult targets = new SimpleItemsNoPagedResult();
        targets.setKind(RestInternalConstants.KIND_COLL_METHODS);

        if (sources == null) {
            targets.setTotal(BigInteger.ZERO);
        } else {
            for (org.siemac.metamac.statistical.operations.core.domain.CollMethod source : sources) {
                SimpleItem target = toSimpleItem(source);
                targets.getItems().add(target);
            }
            targets.setTotal(BigInteger.valueOf(sources.size()));
        }

        return targets;
    }

    @Override
    public SimpleItemsNoPagedResult toCostsNoPagedResult(List<Cost> sources, String apiUrl) {
        SimpleItemsNoPagedResult targets = new SimpleItemsNoPagedResult();
        targets.setKind(RestInternalConstants.KIND_COSTS);

        if (sources == null) {
            targets.setTotal(BigInteger.ZERO);
        } else {
            for (org.siemac.metamac.statistical.operations.core.domain.Cost source : sources) {
                SimpleItem target = toSimpleItem(source);
                targets.getItems().add(target);
            }
            targets.setTotal(BigInteger.valueOf(sources.size()));
        }

        return targets;
    }

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

    private void commonMetadataToOperation(ExternalItem commonMetadata, Operation target) {
        if (commonMetadata == null) {
            return;
        }
        // Calls to CommonMetadata API
        Configuration configuration = commonMetadataRestInternalFacade.retrieveConfigurationById(commonMetadata.getCode());

        // Transform
        target.setContact(configuration.getContact());
        target.setLegalActs(configuration.getLegalActs());
        target.setDataSharing(configuration.getDataSharing());
        target.setConfidentialityPolicy(configuration.getConfPolicy());
        target.setConfidentialityDataTreatment(configuration.getConfDataTreatment());
    }

    private Resource toResource(org.siemac.metamac.statistical.operations.core.domain.Operation source, String apiUrl) {
        if (source == null) {
            return null;
        }
        Resource target = new Resource();
        target.setId(source.getCode());
        target.setUrn(source.getUrn());
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
        target.setUrn(source.getUrn());
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
        target.setUrn(source.getUrn());
        target.setKind(RestInternalConstants.KIND_INSTANCE);
        target.setSelfLink(toInstanceLink(apiUrl, source));
        target.setTitle(toInternationalString(source.getTitle()));
        return target;
    }

    private SimpleItem toSimpleItem(org.siemac.metamac.statistical.operations.core.domain.SurveyType source) {
        if (source == null) {
            return null;
        }
        SimpleItem target = new SimpleItem();
        target.setId(source.getIdentifier());
        target.setTitle(toInternationalString(source.getDescription()));
        return target;
    }

    private SimpleItem toSimpleItem(org.siemac.metamac.statistical.operations.core.domain.OfficialityType source) {
        if (source == null) {
            return null;
        }
        SimpleItem target = new SimpleItem();
        target.setId(source.getIdentifier());
        target.setTitle(toInternationalString(source.getDescription()));
        return target;
    }

    private SimpleItem toSimpleItem(org.siemac.metamac.statistical.operations.core.domain.SurveySource source) {
        if (source == null) {
            return null;
        }
        SimpleItem target = new SimpleItem();
        target.setId(source.getIdentifier());
        target.setTitle(toInternationalString(source.getDescription()));
        return target;
    }

    private SimpleItem toSimpleItem(org.siemac.metamac.statistical.operations.core.domain.InstanceType source) {
        if (source == null) {
            return null;
        }
        SimpleItem target = new SimpleItem();
        target.setId(source.getIdentifier());
        target.setTitle(toInternationalString(source.getDescription()));
        return target;
    }

    private SimpleItem toSimpleItem(org.siemac.metamac.statistical.operations.core.domain.CollMethod source) {
        if (source == null) {
            return null;
        }
        SimpleItem target = new SimpleItem();
        target.setId(source.getIdentifier());
        target.setTitle(toInternationalString(source.getDescription()));
        return target;
    }

    private SimpleItem toSimpleItem(org.siemac.metamac.statistical.operations.core.domain.Cost source) {
        if (source == null) {
            return null;
        }
        SimpleItem target = new SimpleItem();
        target.setId(source.getIdentifier());
        target.setTitle(toInternationalString(source.getDescription()));
        return target;
    }

    private List<SimpleItem> toSimpleItemsCosts(Set<org.siemac.metamac.statistical.operations.core.domain.Cost> sources) {
        List<SimpleItem> targets = new ArrayList<SimpleItem>();
        if (sources == null) {
            return targets;
        }
        for (Cost source : sources) {
            SimpleItem target = toSimpleItem(source);
            targets.add(target);
        }
        return targets;
    }

    private List<Resource> toResourcesExternalItems(Set<ExternalItem> sources, String apiExternalItem) {
        List<Resource> targets = new ArrayList<Resource>();
        if (sources == null) {
            return targets;
        }
        for (ExternalItem source : sources) {
            Resource target = toResourceExternalItem(source, apiExternalItem);
            targets.add(target);
        }
        return targets;
    }

    private List<Resource> toResourcesExternalItemsSrm(Set<ExternalItem> sources) {
        String apiExternalItem = getSrmEndpointInternalApi();
        return toResourcesExternalItems(sources, apiExternalItem);
    }

    private Resource toResourceExternalItemSrm(ExternalItem source) {
        String apiExternalItem = getSrmEndpointInternalApi();
        return toResourceExternalItem(source, apiExternalItem);
    }

    private Resource toResourceExternalItem(ExternalItem source, String apiExternalItem) {
        if (source == null) {
            return null;
        }
        Resource target = new Resource();
        target.setId(source.getCode());
        target.setUrn(source.getUrn());
        target.setKind(source.getType().name());
        target.setSelfLink(RestUtils.createLink(apiExternalItem, source.getUri()));
        target.setTitle(toInternationalString(source.getTitle()));
        return target;
    }

    private ResourceLink toOperationParent(String apiUrl) {
        ResourceLink target = new ResourceLink();
        target.setKind(RestInternalConstants.KIND_OPERATIONS);
        target.setSelfLink(toOperationsLink(apiUrl));
        return target;
    }

    private List<ResourceLink> toOperationChildren(org.siemac.metamac.statistical.operations.core.domain.Operation operation, String apiUrl) {
        List<ResourceLink> targets = new ArrayList<ResourceLink>();

        // Instances
        ResourceLink instancesTarget = new ResourceLink();
        instancesTarget.setKind(RestInternalConstants.KIND_INSTANCES);
        instancesTarget.setSelfLink(toInstancesLink(apiUrl, operation));
        targets.add(instancesTarget);

        // Families
        ResourceLink familiesTarget = new ResourceLink();
        familiesTarget.setKind(RestInternalConstants.KIND_FAMILIES);
        familiesTarget.setSelfLink(toFamiliesByOperationLink(operation, apiUrl));
        targets.add(familiesTarget);

        return targets;
    }

    private ResourceLink toFamilyParent(String apiUrl) {
        ResourceLink target = new ResourceLink();
        target.setKind(RestInternalConstants.KIND_FAMILIES);
        target.setSelfLink(toFamiliesLink(apiUrl));
        return target;
    }

    private List<ResourceLink> toFamilyChildren(org.siemac.metamac.statistical.operations.core.domain.Family family, String apiUrl) {
        List<ResourceLink> targets = new ArrayList<ResourceLink>();

        // Operations of family
        ResourceLink operationsTarget = new ResourceLink();
        operationsTarget.setKind(RestInternalConstants.KIND_OPERATIONS);
        operationsTarget.setSelfLink(toOperationsByFamilyLink(family, apiUrl));
        targets.add(operationsTarget);

        return targets;
    }

    private Resource toInstanceParent(org.siemac.metamac.statistical.operations.core.domain.Instance instance, String apiUrl) {

        // Operation
        Resource target = toResource(instance.getOperation(), apiUrl);

        return target;
    }

    private List<ResourceLink> toInstanceChildren(org.siemac.metamac.statistical.operations.core.domain.Instance instance, String apiUrl) {
        List<ResourceLink> targets = new ArrayList<ResourceLink>();
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

    private org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Status toStatus(StatusEnum source) {
        if (source == null) {
            return null;
        }
        switch (source) {
            case PLANNING:
                return org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Status.PLANNING;
            case DESIGN:
                return org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Status.DESIGN;
            case PRODUCTION:
                return org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Status.PRODUCTION;
            case OUT_OF_PRINT:
                return org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Status.OUT_OF_PRINT;
            default:
                Error error = RestExceptionUtils.getError(RestServiceExceptionType.UNKNOWN);
                throw new RestException(error, Status.INTERNAL_SERVER_ERROR);
        }
    }

    private String getSrmEndpointInternalApi() {
        String property = configurationService.getProperty(RestEndpointsConstants.SRM_INTERNAL_API);
        if (StringUtils.isBlank(property)) {
            Error error = RestExceptionUtils.getError(RestServiceExceptionType.UNKNOWN);
            throw new RestException(error, Status.INTERNAL_SERVER_ERROR);
        }
        return property;
    }
}
