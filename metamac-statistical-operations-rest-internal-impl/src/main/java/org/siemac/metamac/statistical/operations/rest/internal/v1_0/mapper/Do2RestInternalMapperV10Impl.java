package org.siemac.metamac.statistical.operations.rest.internal.v1_0.mapper;

import java.io.Serializable;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Set;

import javax.annotation.PostConstruct;
import javax.ws.rs.core.Response.Status;

import org.fornax.cartridges.sculptor.framework.domain.PagedResult;
import org.joda.time.DateTime;
import org.siemac.metamac.core.common.conf.ConfigurationService;
import org.siemac.metamac.core.common.ent.domain.ExternalItem;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.core.common.exception.MetamacExceptionItem;
import org.siemac.metamac.rest.common.v1_0.domain.Error;
import org.siemac.metamac.rest.common.v1_0.domain.ErrorItem;
import org.siemac.metamac.rest.common.v1_0.domain.ErrorItems;
import org.siemac.metamac.rest.common.v1_0.domain.ErrorParameters;
import org.siemac.metamac.rest.common.v1_0.domain.InternationalString;
import org.siemac.metamac.rest.common.v1_0.domain.Item;
import org.siemac.metamac.rest.common.v1_0.domain.LocalisedString;
import org.siemac.metamac.rest.common.v1_0.domain.Resource;
import org.siemac.metamac.rest.common.v1_0.domain.ResourceLink;
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
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Children;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.ClassSystems;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.CollMethods;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Costs;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Families;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Family;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.FreqColls;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.InformationSuppliers;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Instance;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.InstanceTypes;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Instances;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.OfficialityTypes;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operations;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.ProcStatus;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Producers;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Publishers;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.RegionalContributors;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.RegionalResponsibles;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.SecondarySubjectAreas;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.StatConcDefs;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.StatisticalUnits;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.SurveySources;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.SurveyTypes;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.UnitMeasures;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.UpdateFrequencies;
import org.springframework.beans.factory.BeanCreationException;
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

    private String                           srmApiEndpoint;

    @PostConstruct
    public void init() throws Exception {

        // Srm Api
        srmApiEndpoint = configurationService.getProperty(RestEndpointsConstants.SRM_INTERNAL_API);
        if (srmApiEndpoint == null) {
            throw new BeanCreationException("Property not found: " + RestEndpointsConstants.SRM_INTERNAL_API);
        }
    }

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
        target.setFamilies(toResourcesFamilies(source.getFamilies(), apiUrl));
        target.setSubjectArea(toResourceExternalItemSrm(source.getSubjectArea()));
        target.setSecondarySubjectAreas(toSecondarySubjectAreas(source.getSecondarySubjectAreas()));
        target.setObjective(toInternationalString(source.getObjective()));
        target.setDescription(toInternationalString(source.getDescription()));
//        target.getInstances().addAll(toResourcesInstances(source.getInstances(), apiUrl)); // TODO instances? O sólo como children?
        target.setSurveyType(toItem(source.getSurveyType()));
        target.setOfficialityType(toItem(source.getOfficialityType()));
        target.setIndicatorSystem(source.getIndicatorSystem());
        target.setProducers(toProducers(source.getProducer()));
        target.setRegionalResponsibles(toRegionalResponsibles(source.getRegionalResponsible()));
        target.setRegionalContributors(toRegionalContributors(source.getRegionalContributor()));
        target.setInternalInventoryDate(toDate(source.getInternalInventoryDate()));
        target.setCurrentlyActive(source.getCurrentlyActive());
        target.setStatus(toStatus(source.getStatus()));
        target.setProcStatus(toProcStatus(source.getProcStatus()));
        target.setPublishers(toPublishers(source.getPublisher()));
        target.setRelPolUsAc(toInternationalString(source.getRelPolUsAc()));
        target.setReleaseCalendar(source.getReleaseCalendar());
        target.setReleaseCalendarAccess(source.getReleaseCalendarAccess());
        target.setUpdateFrequencies(toUpdateFrequencies(source.getUpdateFrequency()));
        target.setCurrentInstance(toResource(getInstanceInProcStatus(source.getInstances(), ProcStatusEnum.PUBLISH_EXTERNALLY), apiUrl));
        target.setCurrentInternalInstance(toResource(getInstanceInProcStatus(source.getInstances(), ProcStatusEnum.PUBLISH_INTERNALLY), apiUrl));
        target.setInventoryDate(toDate(source.getInventoryDate()));
        target.setRevPolicy(toInternationalString(source.getRevPolicy()));
        target.setRevPractice(toInternationalString(source.getRevPractice()));
        commonMetadataToOperation(source.getCommonMetadata(), target);
        target.setComment(toInternationalString(source.getComment()));
        target.setNotes(toInternationalString(source.getNotes()));
        target.setParent(toOperationParent(apiUrl));
        target.setChildren(toOperationChildren(source, apiUrl));
        return target;
    }

    @Override
    public Operations toOperations(PagedResult<org.siemac.metamac.statistical.operations.core.domain.Operation> sourcesPagedResult, String query, String orderBy, Integer limit, String apiUrl) {

        Operations targets = new Operations();
        targets.setKind(RestInternalConstants.KIND_OPERATIONS);

        // Pagination
        String baseLink = toOperationsLink(apiUrl);
        SculptorCriteria2RestCriteria.toPagedResult(sourcesPagedResult, targets, query, orderBy, limit, baseLink);

        // Values
        for (org.siemac.metamac.statistical.operations.core.domain.Operation source : sourcesPagedResult.getValues()) {
            Resource target = toResource(source, apiUrl);
            targets.getOperations().add(target);
        }
        return targets;
    }

    @Override
    public Operations toOperationsByFamily(org.siemac.metamac.statistical.operations.core.domain.Family family,
            PagedResult<org.siemac.metamac.statistical.operations.core.domain.Operation> sourcesPagedResult, String query, String orderBy, Integer limit, String apiUrl) {

        Operations targets = new Operations();
        targets.setKind(RestInternalConstants.KIND_OPERATIONS);

        // Pagination
        String baseLink = toOperationsByFamilyLink(family, apiUrl);
        SculptorCriteria2RestCriteria.toPagedResult(sourcesPagedResult, targets, query, orderBy, limit, baseLink);

        // Values
        for (org.siemac.metamac.statistical.operations.core.domain.Operation source : sourcesPagedResult.getValues()) {
            Resource target = toResource(source, apiUrl);
            targets.getOperations().add(target);
        }
        return targets;
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
        target.setChildren(toFamilyChildren(source, apiUrl));
        return target;
    }

    @Override
    public Families toFamilies(PagedResult<org.siemac.metamac.statistical.operations.core.domain.Family> sourcesPagedResult, String query, String orderBy, Integer limit, String apiUrl) {

        Families targets = new Families();
        targets.setKind(RestInternalConstants.KIND_FAMILIES);

        // Pagination
        String baseLink = toFamiliesLink(apiUrl);
        SculptorCriteria2RestCriteria.toPagedResult(sourcesPagedResult, targets, query, orderBy, limit, baseLink);

        // Values
        for (org.siemac.metamac.statistical.operations.core.domain.Family source : sourcesPagedResult.getValues()) {
            Resource target = toResource(source, apiUrl);
            targets.getFamilies().add(target);
        }
        return targets;
    }

    @Override
    public Families toFamiliesByOperation(List<org.siemac.metamac.statistical.operations.core.domain.Family> sources, String apiUrl) {

        Families targets = new Families();
        targets.setKind(RestInternalConstants.KIND_FAMILIES);

        if (sources == null) {
            targets.setTotal(BigInteger.ZERO);
        } else {
            for (org.siemac.metamac.statistical.operations.core.domain.Family source : sources) {
                Resource target = toResource(source, apiUrl);
                targets.getFamilies().add(target);
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
        target.setStatisticalUnits(toStatisticalUnits(source.getStatisticalUnit()));
        target.setGeographicGranularity(toResourceExternalItemSrm(source.getGeographicGranularity()));
        target.setGeographicComparability(toInternationalString(source.getGeographicComparability()));
        target.setTemporalGranularity(toResourceExternalItemSrm(source.getTemporalGranularity()));
        target.setTemporalComparability(toInternationalString(source.getTemporalComparability()));
        target.setBasePeriod(source.getBasePeriod());
        target.setUnitMeasures(toUnitMeasures(source.getUnitMeasure()));
        target.setStatConcDefsDescription(toInternationalString(source.getStatConcDef()));
        target.setStatConcDefs(toStatConcDefs(source.getStatConcDefList()));
        target.setClassSystemsDescription(toInternationalString(source.getClassSystem()));
        target.setClassSystems(toClassSystems(source.getClassSystemList()));
        target.setInstanceType(toItem(source.getInstanceType()));
        target.setInternalInventoryDate(toDate(source.getInternalInventoryDate()));
        target.setProcStatus(toProcStatus(source.getProcStatus()));
        target.setDocMethod(toInternationalString(source.getDocMethod()));
        target.setSurveySource(toItem(source.getSurveySource()));
        target.setCollMethod(toItem(source.getCollMethod()));
        target.setInformationSuppliers(toInformationSuppliers(source.getInformationSuppliers()));
        target.setFreqColls(toFreqColls(source.getFreqColl()));
        target.setDataValidation(toInternationalString(source.getDataValidation()));
        target.setDataCompilation(toInternationalString(source.getDataCompilation()));
        target.setAdjustment(toInternationalString(source.getAdjustment()));
        target.setCostBurden(toInternationalString(source.getCostBurden()));
        target.setCosts(toCosts(source.getCost()));
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
        target.setChildren(toInstanceChildren(source, apiUrl));
        return target;
    }

    @Override
    public Instances toInstances(org.siemac.metamac.statistical.operations.core.domain.Operation operation,
            PagedResult<org.siemac.metamac.statistical.operations.core.domain.Instance> sourcesPagedResult, String query, String orderBy, Integer limit, String apiUrl) {

        Instances targets = new Instances();
        targets.setKind(RestInternalConstants.KIND_INSTANCES);

        // Pagination
        String baseLink = toInstancesLink(apiUrl, operation);
        SculptorCriteria2RestCriteria.toPagedResult(sourcesPagedResult, targets, query, orderBy, limit, baseLink);

        // Values
        for (org.siemac.metamac.statistical.operations.core.domain.Instance source : sourcesPagedResult.getValues()) {
            Resource target = toResource(source, apiUrl);
            targets.getInstances().add(target);
        }
        return targets;
    }

    @Override
    public SurveyTypes toSurveyTypes(List<SurveyType> sources) {
        SurveyTypes targets = new SurveyTypes();
        targets.setKind(RestInternalConstants.KIND_SURVEY_TYPES);

        if (sources == null) {
            targets.setTotal(BigInteger.ZERO);
        } else {
            for (org.siemac.metamac.statistical.operations.core.domain.SurveyType source : sources) {
                Item target = toItem(source);
                targets.getSurveyTypes().add(target);
            }
            targets.setTotal(BigInteger.valueOf(sources.size()));
        }

        return targets;
    }

    @Override
    public OfficialityTypes toOfficialityTypes(List<OfficialityType> sources) {
        OfficialityTypes targets = new OfficialityTypes();
        targets.setKind(RestInternalConstants.KIND_OFFICIALITY_TYPES);

        if (sources == null) {
            targets.setTotal(BigInteger.ZERO);
        } else {
            for (org.siemac.metamac.statistical.operations.core.domain.OfficialityType source : sources) {
                Item target = toItem(source);
                targets.getOfficialityTypes().add(target);
            }
            targets.setTotal(BigInteger.valueOf(sources.size()));
        }

        return targets;
    }

    @Override
    public InstanceTypes toInstanceTypes(List<InstanceType> sources) {
        InstanceTypes targets = new InstanceTypes();
        targets.setKind(RestInternalConstants.KIND_INSTANCE_TYPES);

        if (sources == null) {
            targets.setTotal(BigInteger.ZERO);
        } else {
            for (org.siemac.metamac.statistical.operations.core.domain.InstanceType source : sources) {
                Item target = toItem(source);
                targets.getInstanceTypes().add(target);
            }
            targets.setTotal(BigInteger.valueOf(sources.size()));
        }

        return targets;
    }

    @Override
    public SurveySources toSurveySources(List<SurveySource> sources) {
        SurveySources targets = new SurveySources();
        targets.setKind(RestInternalConstants.KIND_SURVEY_SOURCES);

        if (sources == null) {
            targets.setTotal(BigInteger.ZERO);
        } else {
            for (org.siemac.metamac.statistical.operations.core.domain.SurveySource source : sources) {
                Item target = toItem(source);
                targets.getSurveySources().add(target);
            }
            targets.setTotal(BigInteger.valueOf(sources.size()));
        }

        return targets;
    }

    @Override
    public CollMethods toCollMethods(List<CollMethod> sources) {
        CollMethods targets = new CollMethods();
        targets.setKind(RestInternalConstants.KIND_COLL_METHODS);

        if (sources == null) {
            targets.setTotal(BigInteger.ZERO);
        } else {
            for (org.siemac.metamac.statistical.operations.core.domain.CollMethod source : sources) {
                Item target = toItem(source);
                targets.getCollMethods().add(target);
            }
            targets.setTotal(BigInteger.valueOf(sources.size()));
        }

        return targets;
    }

    @Override
    public Costs toCosts(List<Cost> sources) {
        return toCosts((Collection<Cost>) sources);
    }

    @Override
    public Error toError(Exception exception) {
        Error error = new Error();
        error.setErrorItems(toErrorItems(exception));
        return error;
    }

    private Costs toCosts(Collection<Cost> sources) {
        Costs targets = new Costs();
        targets.setKind(RestInternalConstants.KIND_COSTS);

        if (sources == null) {
            targets.setTotal(BigInteger.ZERO);
        } else {
            for (org.siemac.metamac.statistical.operations.core.domain.Cost source : sources) {
                Item target = toItem(source);
                targets.getCosts().add(target);
            }
            targets.setTotal(BigInteger.valueOf(sources.size()));
        }

        return targets;
    }

    private ErrorItems toErrorItems(Exception exception) {
        ErrorItems errorItemList = new ErrorItems();
        if (exception instanceof MetamacException) {
            MetamacException metamacException = (MetamacException) exception;
            for (MetamacExceptionItem metamacExceptionItem : metamacException.getExceptionItems()) {
                ErrorItem errorItem = new ErrorItem();
                errorItem.setCode(metamacExceptionItem.getCode());
                errorItem.setMessage(metamacExceptionItem.getMessage());
                errorItem.setParameters(new ErrorParameters());
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
                        errorItem.getParameters().getParameters().add(parameter);
                    }
                }
                errorItemList.getErrorItems().add(errorItem);
            }
        } else {
            ErrorItem errorItem = RestExceptionUtils.getErrorItem(RestCommonServiceExceptionType.UNKNOWN);
            errorItemList.getErrorItems().add(errorItem);
        }
        return errorItemList;
    }

    private void commonMetadataToOperation(ExternalItem commonMetadata, Operation target) {
        if (commonMetadata == null) {
            return;
        }
        // // Calls to CommonMetadata API TODO
        // Configuration configuration = commonMetadataRestInternalFacade.retrieveConfigurationById(commonMetadata.getCode());
        //
        // // Transform
        // target.setContact(configuration.getContact());
        // target.setLegalActs(configuration.getLegalActs());
        // target.setDataSharing(configuration.getDataSharing());
        // target.setConfidentialityPolicy(configuration.getConfPolicy());
        // target.setConfidentialityDataTreatment(configuration.getConfDataTreatment());
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

    private Families toResourcesFamilies(Set<org.siemac.metamac.statistical.operations.core.domain.Family> sources, String apiUrl) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        Families targets = new Families();
        for (org.siemac.metamac.statistical.operations.core.domain.Family source : sources) {
            Resource target = toResource(source, apiUrl);
            targets.getFamilies().add(target);
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

    private Item toItem(org.siemac.metamac.statistical.operations.core.domain.SurveyType source) {
        if (source == null) {
            return null;
        }
        Item target = new Item();
        target.setId(source.getIdentifier());
        target.setTitle(toInternationalString(source.getDescription()));
        return target;
    }

    private Item toItem(org.siemac.metamac.statistical.operations.core.domain.OfficialityType source) {
        if (source == null) {
            return null;
        }
        Item target = new Item();
        target.setId(source.getIdentifier());
        target.setTitle(toInternationalString(source.getDescription()));
        return target;
    }

    private Item toItem(org.siemac.metamac.statistical.operations.core.domain.SurveySource source) {
        if (source == null) {
            return null;
        }
        Item target = new Item();
        target.setId(source.getIdentifier());
        target.setTitle(toInternationalString(source.getDescription()));
        return target;
    }

    private Item toItem(org.siemac.metamac.statistical.operations.core.domain.InstanceType source) {
        if (source == null) {
            return null;
        }
        Item target = new Item();
        target.setId(source.getIdentifier());
        target.setTitle(toInternationalString(source.getDescription()));
        return target;
    }

    private Item toItem(org.siemac.metamac.statistical.operations.core.domain.CollMethod source) {
        if (source == null) {
            return null;
        }
        Item target = new Item();
        target.setId(source.getIdentifier());
        target.setTitle(toInternationalString(source.getDescription()));
        return target;
    }

    private Item toItem(org.siemac.metamac.statistical.operations.core.domain.Cost source) {
        if (source == null) {
            return null;
        }
        Item target = new Item();
        target.setId(source.getIdentifier());
        target.setTitle(toInternationalString(source.getDescription()));
        return target;
    }

    private List<Item> toItemsCosts(Set<org.siemac.metamac.statistical.operations.core.domain.Cost> sources) {
        List<Item> targets = new ArrayList<Item>();
        if (sources == null) {
            return targets;
        }
        for (Cost source : sources) {
            Item target = toItem(source);
            targets.add(target);
        }
        return targets;
    }

    private void toResourcesExternalItems(Set<ExternalItem> sources, List<Resource> targets, String apiExternalItem) {
        if (sources == null) {
            return;
        }
        for (ExternalItem source : sources) {
            Resource target = toResourceExternalItem(source, apiExternalItem);
            targets.add(target);
        }
    }

    private void toResourcesExternalItemsSrm(Set<ExternalItem> sources, List<Resource> targets) {
        toResourcesExternalItems(sources, targets, srmApiEndpoint);
    }

    private Resource toResourceExternalItemSrm(ExternalItem source) {
        if (source == null) {
            return null;
        }
        return toResourceExternalItem(source, srmApiEndpoint);
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

    private Children toOperationChildren(org.siemac.metamac.statistical.operations.core.domain.Operation operation, String apiUrl) {
        Children targets = new Children();

        // Instances
        ResourceLink instancesTarget = new ResourceLink();
        instancesTarget.setKind(RestInternalConstants.KIND_INSTANCES);
        instancesTarget.setSelfLink(toInstancesLink(apiUrl, operation));
        targets.getChildren().add(instancesTarget);

        // Families
        ResourceLink familiesTarget = new ResourceLink();
        familiesTarget.setKind(RestInternalConstants.KIND_FAMILIES);
        familiesTarget.setSelfLink(toFamiliesByOperationLink(operation, apiUrl));
        targets.getChildren().add(familiesTarget);

        return targets;
    }

    private ResourceLink toFamilyParent(String apiUrl) {
        ResourceLink target = new ResourceLink();
        target.setKind(RestInternalConstants.KIND_FAMILIES);
        target.setSelfLink(toFamiliesLink(apiUrl));
        return target;
    }

    private Children toFamilyChildren(org.siemac.metamac.statistical.operations.core.domain.Family family, String apiUrl) {
        Children targets = new Children();

        // Operations of family
        ResourceLink operationsTarget = new ResourceLink();
        operationsTarget.setKind(RestInternalConstants.KIND_OPERATIONS);
        operationsTarget.setSelfLink(toOperationsByFamilyLink(family, apiUrl));
        targets.getChildren().add(operationsTarget);

        return targets;
    }

    private Resource toInstanceParent(org.siemac.metamac.statistical.operations.core.domain.Instance instance, String apiUrl) {

        // Operation
        Resource target = toResource(instance.getOperation(), apiUrl);

        return target;
    }

    private Children toInstanceChildren(org.siemac.metamac.statistical.operations.core.domain.Instance instance, String apiUrl) {
        // No children
        return null;
    }

    private InternationalString toInternationalString(org.siemac.metamac.core.common.ent.domain.InternationalString sources) {
        if (sources == null) {
            return null;
        }
        InternationalString targets = new InternationalString();
        for (org.siemac.metamac.core.common.ent.domain.LocalisedString source : sources.getTexts()) {
            LocalisedString target = new LocalisedString();
            target.setValue(source.getLabel());
            target.setLang(source.getLocale());
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
    
    private SecondarySubjectAreas toSecondarySubjectAreas(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        SecondarySubjectAreas targets = new SecondarySubjectAreas();
        toResourcesExternalItemsSrm(sources, targets.getSecondarySubjectAreas());
        return targets;
    }
    
    private Producers toProducers(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        Producers targets = new Producers();
        toResourcesExternalItemsSrm(sources, targets.getProducers());
        return targets;
    }
    
    private RegionalResponsibles toRegionalResponsibles(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        RegionalResponsibles targets = new RegionalResponsibles();
        toResourcesExternalItemsSrm(sources, targets.getRegionalResponsibles());
        return targets;
    }
    
    private RegionalContributors toRegionalContributors(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        RegionalContributors targets = new RegionalContributors();
        toResourcesExternalItemsSrm(sources, targets.getRegionalContributors());
        return targets;
    }
    
    private Publishers toPublishers(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        Publishers targets = new Publishers();
        toResourcesExternalItemsSrm(sources, targets.getPublishers());
        return targets;
    }
    
    private UpdateFrequencies toUpdateFrequencies(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        UpdateFrequencies targets = new UpdateFrequencies();
        toResourcesExternalItemsSrm(sources, targets.getUpdateFrequencies());
        return targets;
    }
    
    private StatisticalUnits toStatisticalUnits(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        StatisticalUnits targets = new StatisticalUnits();
        toResourcesExternalItemsSrm(sources, targets.getStatisticalUnits());
        return targets;
    }

    private UnitMeasures toUnitMeasures(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        UnitMeasures targets = new UnitMeasures();
        toResourcesExternalItemsSrm(sources, targets.getUnitMeasures());
        return targets;
    }

    private StatConcDefs toStatConcDefs(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        StatConcDefs targets = new StatConcDefs();
        toResourcesExternalItemsSrm(sources, targets.getStatConcDefs());
        return targets;
    }

    private ClassSystems toClassSystems(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        ClassSystems targets = new ClassSystems();
        toResourcesExternalItemsSrm(sources, targets.getClassSystems());
        return targets;
    }

    private InformationSuppliers toInformationSuppliers(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        InformationSuppliers targets = new InformationSuppliers();
        toResourcesExternalItemsSrm(sources, targets.getInformationSuppliers());
        return targets;
    }

    private FreqColls toFreqColls(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        FreqColls targets = new FreqColls();
        toResourcesExternalItemsSrm(sources, targets.getFreqColls());
        return targets;
    }
}
