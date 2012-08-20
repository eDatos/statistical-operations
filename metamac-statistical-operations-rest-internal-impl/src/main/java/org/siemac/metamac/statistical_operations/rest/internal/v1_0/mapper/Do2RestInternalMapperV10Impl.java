package org.siemac.metamac.statistical_operations.rest.internal.v1_0.mapper;

import java.math.BigInteger;
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
import org.siemac.metamac.rest.common.v1_0.domain.Children;
import org.siemac.metamac.rest.common.v1_0.domain.InternationalString;
import org.siemac.metamac.rest.common.v1_0.domain.Item;
import org.siemac.metamac.rest.common.v1_0.domain.LocalisedString;
import org.siemac.metamac.rest.common.v1_0.domain.Resource;
import org.siemac.metamac.rest.common.v1_0.domain.ResourceLink;
import org.siemac.metamac.rest.common_metadata.v1_0.domain.Configuration;
import org.siemac.metamac.rest.constants.RestEndpointsConstants;
import org.siemac.metamac.rest.exception.RestException;
import org.siemac.metamac.rest.exception.utils.RestExceptionUtils;
import org.siemac.metamac.rest.search.criteria.mapper.SculptorCriteria2RestCriteria;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.ClassSystems;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.CollMethods;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.Costs;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.Families;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.Family;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.FreqColls;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.InformationSuppliers;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.Instance;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.InstanceTypes;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.Instances;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.OfficialityTypes;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.Operation;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.Operations;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.ProcStatus;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.Producers;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.Publishers;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.RegionalContributors;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.RegionalResponsibles;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.SecondarySubjectAreas;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.StatConcDefs;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.StatisticalUnits;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.SurveySources;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.SurveyTypes;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.UnitMeasures;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.UpdateFrequencies;
import org.siemac.metamac.rest.utils.RestUtils;
import org.siemac.metamac.statistical.operations.core.domain.CollMethod;
import org.siemac.metamac.statistical.operations.core.domain.Cost;
import org.siemac.metamac.statistical.operations.core.domain.InstanceType;
import org.siemac.metamac.statistical.operations.core.domain.OfficialityType;
import org.siemac.metamac.statistical.operations.core.domain.SurveySource;
import org.siemac.metamac.statistical.operations.core.domain.SurveyType;
import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.core.enume.domain.StatusEnum;
import org.siemac.metamac.statistical_operations.rest.internal.RestInternalConstants;
import org.siemac.metamac.statistical_operations.rest.internal.exception.RestServiceExceptionType;
import org.siemac.metamac.statistical_operations.rest.internal.invocation.CommonMetadataRestExternalFacade;
import org.springframework.beans.factory.BeanCreationException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class Do2RestInternalMapperV10Impl implements Do2RestInternalMapperV10 {

    @Autowired
    private ConfigurationService             configurationService;

    @Autowired
    private CommonMetadataRestExternalFacade commonMetadataRestInternalFacade;

    private String                           statisticalOperationsApiInternalEndpointV10;
    private String                           srmApiInternalEndpoint;

    public static String                     KIND_SRM_EXTERNAL_ITEM = "PENDIENTE_API_SRM"; // TODO METAMAC-916

    @PostConstruct
    public void init() throws Exception {
        // Statistical operations Internal Api
        String statisticalOperationsApiInternalEndpoint = configurationService.getProperty(RestEndpointsConstants.STATISTICAL_OPERATIONS_INTERNAL_API);
        if (statisticalOperationsApiInternalEndpoint == null) {
            throw new BeanCreationException("Property not found: " + RestEndpointsConstants.STATISTICAL_OPERATIONS_INTERNAL_API);
        }
        statisticalOperationsApiInternalEndpointV10 = RestUtils.createLink(statisticalOperationsApiInternalEndpoint, RestInternalConstants.API_VERSION_1_0);
        
        // Srm Internal Api
        srmApiInternalEndpoint = configurationService.getProperty(RestEndpointsConstants.SRM_INTERNAL_API);
        if (srmApiInternalEndpoint == null) {
            throw new BeanCreationException("Property not found: " + RestEndpointsConstants.SRM_INTERNAL_API);
        }
    }

    @Override
    public Operation toOperation(org.siemac.metamac.statistical.operations.core.domain.Operation source) {
        if (source == null) {
            return null;
        }
        Operation target = new Operation();
        target.setId(source.getCode());
        target.setUrn(source.getUrn());
        target.setKind(RestInternalConstants.KIND_OPERATION);
        target.setSelfLink(toOperationLink(source));
        target.setTitle(toInternationalString(source.getTitle()));
        target.setAcronym(toInternationalString(source.getAcronym()));
        target.setSubjectArea(toResourceExternalItemSrm(source.getSubjectArea()));
        target.setSecondarySubjectAreas(toSecondarySubjectAreas(source.getSecondarySubjectAreas()));
        target.setObjective(toInternationalString(source.getObjective()));
        target.setDescription(toInternationalString(source.getDescription()));
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
        target.setCurrentInstance(toResource(getInstanceInProcStatus(source.getInstances(), ProcStatusEnum.PUBLISH_EXTERNALLY)));
        target.setCurrentInternalInstance(toResource(getInstanceInProcStatus(source.getInstances(), ProcStatusEnum.PUBLISH_INTERNALLY)));
        target.setInventoryDate(toDate(source.getInventoryDate()));
        target.setRevPolicy(toInternationalString(source.getRevPolicy()));
        target.setRevPractice(toInternationalString(source.getRevPractice()));
        commonMetadataToOperation(source.getCommonMetadata(), target);
        target.setComment(toInternationalString(source.getComment()));
        target.setNotes(toInternationalString(source.getNotes()));
        target.setParent(toOperationParent());
        target.setChildren(toOperationChildren(source));
        return target;
    }

    @Override
    public Operations toOperations(PagedResult<org.siemac.metamac.statistical.operations.core.domain.Operation> sourcesPagedResult, String query, String orderBy, Integer limit) {

        Operations targets = new Operations();
        targets.setKind(RestInternalConstants.KIND_OPERATIONS);

        // Pagination
        String baseLink = toOperationsLink();
        SculptorCriteria2RestCriteria.toPagedResult(sourcesPagedResult, targets, query, orderBy, limit, baseLink);

        // Values
        for (org.siemac.metamac.statistical.operations.core.domain.Operation source : sourcesPagedResult.getValues()) {
            Resource target = toResource(source);
            targets.getOperations().add(target);
        }
        return targets;
    }

    @Override
    public Operations toOperationsByFamily(org.siemac.metamac.statistical.operations.core.domain.Family family,
            PagedResult<org.siemac.metamac.statistical.operations.core.domain.Operation> sourcesPagedResult, String query, String orderBy, Integer limit) {

        Operations targets = new Operations();
        targets.setKind(RestInternalConstants.KIND_OPERATIONS);

        // Pagination
        String baseLink = toOperationsByFamilyLink(family);
        SculptorCriteria2RestCriteria.toPagedResult(sourcesPagedResult, targets, query, orderBy, limit, baseLink);

        // Values
        for (org.siemac.metamac.statistical.operations.core.domain.Operation source : sourcesPagedResult.getValues()) {
            Resource target = toResource(source);
            targets.getOperations().add(target);
        }
        return targets;
    }

    @Override
    public Family toFamily(org.siemac.metamac.statistical.operations.core.domain.Family source) {
        if (source == null) {
            return null;
        }
        Family target = new Family();
        target.setId(source.getCode());
        target.setUrn(source.getUrn());
        target.setKind(RestInternalConstants.KIND_FAMILY);
        target.setSelfLink(toFamilyLink(source));
        target.setTitle(toInternationalString(source.getTitle()));
        target.setAcronym(toInternationalString(source.getAcronym()));
        target.setDescription(toInternationalString(source.getDescription()));
        target.setInternalInventoryDate(toDate(source.getInternalInventoryDate()));
        target.setProcStatus(toProcStatus(source.getProcStatus()));
        target.setInventoryDate(toDate(source.getInventoryDate()));
        target.setParent(toFamilyParent());
        target.setChildren(toFamilyChildren(source));
        return target;
    }

    @Override
    public Families toFamilies(PagedResult<org.siemac.metamac.statistical.operations.core.domain.Family> sourcesPagedResult, String query, String orderBy, Integer limit) {

        Families targets = new Families();
        targets.setKind(RestInternalConstants.KIND_FAMILIES);

        // Pagination
        String baseLink = toFamiliesLink();
        SculptorCriteria2RestCriteria.toPagedResult(sourcesPagedResult, targets, query, orderBy, limit, baseLink);

        // Values
        for (org.siemac.metamac.statistical.operations.core.domain.Family source : sourcesPagedResult.getValues()) {
            Resource target = toResource(source);
            targets.getFamilies().add(target);
        }
        return targets;
    }

    @Override
    public Families toFamiliesByOperation(List<org.siemac.metamac.statistical.operations.core.domain.Family> sources) {

        Families targets = new Families();
        targets.setKind(RestInternalConstants.KIND_FAMILIES);

        if (sources == null) {
            targets.setTotal(BigInteger.ZERO);
        } else {
            for (org.siemac.metamac.statistical.operations.core.domain.Family source : sources) {
                Resource target = toResource(source);
                targets.getFamilies().add(target);
            }
            targets.setTotal(BigInteger.valueOf(sources.size()));
        }

        return targets;
    }

    @Override
    public Instance toInstance(org.siemac.metamac.statistical.operations.core.domain.Instance source) {
        if (source == null) {
            return null;
        }
        Instance target = new Instance();
        target.setId(source.getCode());
        target.setUrn(source.getUrn());
        target.setKind(RestInternalConstants.KIND_INSTANCE);
        target.setSelfLink(toInstanceLink(source));
        target.setTitle(toInternationalString(source.getTitle()));
        target.setAcronym(toInternationalString(source.getAcronym()));
        target.setSurvey(toResource(source.getOperation()));
        target.setPredecessor(toResource(getInstanceInOrder(source.getOperation().getInstances(), source.getOrder() - 1)));
        target.setSuccessor(toResource(getInstanceInOrder(source.getOperation().getInstances(), source.getOrder() + 1)));
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
        target.setParent(toInstanceParent(source));
        target.setChildren(toInstanceChildren(source));
        return target;
    }

    @Override
    public Instances toInstances(org.siemac.metamac.statistical.operations.core.domain.Operation operation,
            PagedResult<org.siemac.metamac.statistical.operations.core.domain.Instance> sourcesPagedResult, String query, String orderBy, Integer limit) {

        Instances targets = new Instances();
        targets.setKind(RestInternalConstants.KIND_INSTANCES);

        // Pagination
        String baseLink = toInstancesLink(operation);
        SculptorCriteria2RestCriteria.toPagedResult(sourcesPagedResult, targets, query, orderBy, limit, baseLink);

        // Values
        for (org.siemac.metamac.statistical.operations.core.domain.Instance source : sourcesPagedResult.getValues()) {
            Resource target = toResource(source);
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

    private Resource toResource(org.siemac.metamac.statistical.operations.core.domain.Operation source) {
        if (source == null) {
            return null;
        }
        Resource target = new Resource();
        target.setId(source.getCode());
        target.setUrn(source.getUrn());
        target.setKind(RestInternalConstants.KIND_OPERATION);
        target.setSelfLink(toOperationLink(source));
        target.setTitle(toInternationalString(source.getTitle()));
        return target;
    }

    private Resource toResource(org.siemac.metamac.statistical.operations.core.domain.Family source) {
        if (source == null) {
            return null;
        }
        Resource target = new Resource();
        target.setId(source.getCode());
        target.setUrn(source.getUrn());
        target.setKind(RestInternalConstants.KIND_FAMILY);
        target.setSelfLink(toFamilyLink(source));
        target.setTitle(toInternationalString(source.getTitle()));
        return target;
    }

    private Resource toResource(org.siemac.metamac.statistical.operations.core.domain.Instance source) {
        if (source == null) {
            return null;
        }
        Resource target = new Resource();
        target.setId(source.getCode());
        target.setUrn(source.getUrn());
        target.setKind(RestInternalConstants.KIND_INSTANCE);
        target.setSelfLink(toInstanceLink(source));
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
        toResourcesExternalItems(sources, targets, srmApiInternalEndpoint);
    }

    private Resource toResourceExternalItemSrm(ExternalItem source) {
        if (source == null) {
            return null;
        }
        return toResourceExternalItem(source, srmApiInternalEndpoint);
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

    private ResourceLink toOperationParent() {
        ResourceLink target = new ResourceLink();
        target.setKind(RestInternalConstants.KIND_OPERATIONS);
        target.setSelfLink(toOperationsLink());
        return target;
    }

    private Children toOperationChildren(org.siemac.metamac.statistical.operations.core.domain.Operation operation) {
        Children targets = new Children();

        // Instances
        ResourceLink instancesTarget = new ResourceLink();
        instancesTarget.setKind(RestInternalConstants.KIND_INSTANCES);
        instancesTarget.setSelfLink(toInstancesLink(operation));
        targets.getChildren().add(instancesTarget);

        // Families
        ResourceLink familiesTarget = new ResourceLink();
        familiesTarget.setKind(RestInternalConstants.KIND_FAMILIES);
        familiesTarget.setSelfLink(toFamiliesByOperationLink(operation));
        targets.getChildren().add(familiesTarget);

        targets.setTotal(BigInteger.valueOf(targets.getChildren().size()));
        return targets;
    }

    private ResourceLink toFamilyParent() {
        ResourceLink target = new ResourceLink();
        target.setKind(RestInternalConstants.KIND_FAMILIES);
        target.setSelfLink(toFamiliesLink());
        return target;
    }

    private Children toFamilyChildren(org.siemac.metamac.statistical.operations.core.domain.Family family) {
        Children targets = new Children();

        // Operations of family
        ResourceLink operationsTarget = new ResourceLink();
        operationsTarget.setKind(RestInternalConstants.KIND_OPERATIONS);
        operationsTarget.setSelfLink(toOperationsByFamilyLink(family));
        targets.getChildren().add(operationsTarget);

        targets.setTotal(BigInteger.valueOf(targets.getChildren().size()));
        return targets;
    }

    private Resource toInstanceParent(org.siemac.metamac.statistical.operations.core.domain.Instance instance) {

        // Operation
        Resource target = toResource(instance.getOperation());

        return target;
    }

    private Children toInstanceChildren(org.siemac.metamac.statistical.operations.core.domain.Instance instance) {
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
    private String toOperationsLink() {
        return RestUtils.createLink(statisticalOperationsApiInternalEndpointV10, RestInternalConstants.LINK_SUBPATH_OPERATIONS);
    }

    // API/operations/OPERATION_ID
    private String toOperationLink(org.siemac.metamac.statistical.operations.core.domain.Operation operation) {
        String linkOperations = toOperationsLink();
        return RestUtils.createLink(linkOperations, operation.getCode());
    }

    // API/operations/OPERATION_ID/instances
    private String toInstancesLink(org.siemac.metamac.statistical.operations.core.domain.Operation operation) {
        String linkOperation = toOperationLink(operation);
        return RestUtils.createLink(linkOperation, RestInternalConstants.LINK_SUBPATH_INSTANCES);
    }

    // API/operations/OPERATION_ID/instances/INSTANCE_ID
    private String toInstanceLink(org.siemac.metamac.statistical.operations.core.domain.Instance instance) {
        String linkOperation = toInstancesLink(instance.getOperation());
        return RestUtils.createLink(linkOperation, instance.getCode());
    }

    // API/families
    private String toFamiliesLink() {
        return RestUtils.createLink(statisticalOperationsApiInternalEndpointV10, RestInternalConstants.LINK_SUBPATH_FAMILIES);
    }

    // API/families/family
    private String toFamilyLink(org.siemac.metamac.statistical.operations.core.domain.Family family) {
        String linkFamilies = toFamiliesLink();
        return RestUtils.createLink(linkFamilies, family.getCode());
    }

    // API/operations/OPERATION_ID/families
    private String toFamiliesByOperationLink(org.siemac.metamac.statistical.operations.core.domain.Operation operation) {
        String linkFamily = toOperationLink(operation);
        return RestUtils.createLink(linkFamily, RestInternalConstants.LINK_SUBPATH_FAMILIES);
    }

    // API/families/FAMILY_ID/operations
    private String toOperationsByFamilyLink(org.siemac.metamac.statistical.operations.core.domain.Family family) {
        String linkFamily = toFamilyLink(family);
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
                org.siemac.metamac.rest.common.v1_0.domain.Exception exception = RestExceptionUtils.getException(RestServiceExceptionType.UNKNOWN);
                throw new RestException(exception, Status.INTERNAL_SERVER_ERROR);
        }
    }

    private org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.Status toStatus(StatusEnum source) {
        if (source == null) {
            return null;
        }
        switch (source) {
            case PLANNING:
                return org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.Status.PLANNING;
            case DESIGN:
                return org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.Status.DESIGN;
            case PRODUCTION:
                return org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.Status.PRODUCTION;
            case OUT_OF_PRINT:
                return org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.Status.OUT_OF_PRINT;
            default:
                org.siemac.metamac.rest.common.v1_0.domain.Exception exception = RestExceptionUtils.getException(RestServiceExceptionType.UNKNOWN);
                throw new RestException(exception, Status.INTERNAL_SERVER_ERROR);
        }
    }

    private SecondarySubjectAreas toSecondarySubjectAreas(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        SecondarySubjectAreas targets = new SecondarySubjectAreas();
        toResourcesExternalItemsSrm(sources, targets.getSecondarySubjectAreas());
        targets.setKind(KIND_SRM_EXTERNAL_ITEM);
        targets.setTotal(BigInteger.valueOf(targets.getSecondarySubjectAreas().size()));
        return targets;
    }

    private Producers toProducers(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        Producers targets = new Producers();
        toResourcesExternalItemsSrm(sources, targets.getProducers());
        targets.setKind(KIND_SRM_EXTERNAL_ITEM);
        targets.setTotal(BigInteger.valueOf(targets.getProducers().size()));
        return targets;
    }

    private RegionalResponsibles toRegionalResponsibles(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        RegionalResponsibles targets = new RegionalResponsibles();
        toResourcesExternalItemsSrm(sources, targets.getRegionalResponsibles());
        targets.setKind(KIND_SRM_EXTERNAL_ITEM);
        targets.setTotal(BigInteger.valueOf(targets.getRegionalResponsibles().size()));
        return targets;
    }

    private RegionalContributors toRegionalContributors(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        RegionalContributors targets = new RegionalContributors();
        toResourcesExternalItemsSrm(sources, targets.getRegionalContributors());
        targets.setKind(KIND_SRM_EXTERNAL_ITEM);
        targets.setTotal(BigInteger.valueOf(targets.getRegionalContributors().size()));
        return targets;
    }

    private Publishers toPublishers(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        Publishers targets = new Publishers();
        toResourcesExternalItemsSrm(sources, targets.getPublishers());
        targets.setKind(KIND_SRM_EXTERNAL_ITEM);
        targets.setTotal(BigInteger.valueOf(targets.getPublishers().size()));
        return targets;
    }

    private UpdateFrequencies toUpdateFrequencies(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        UpdateFrequencies targets = new UpdateFrequencies();
        toResourcesExternalItemsSrm(sources, targets.getUpdateFrequencies());
        targets.setKind(KIND_SRM_EXTERNAL_ITEM);
        targets.setTotal(BigInteger.valueOf(targets.getUpdateFrequencies().size()));
        return targets;
    }

    private StatisticalUnits toStatisticalUnits(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        StatisticalUnits targets = new StatisticalUnits();
        toResourcesExternalItemsSrm(sources, targets.getStatisticalUnits());
        targets.setKind(KIND_SRM_EXTERNAL_ITEM);
        targets.setTotal(BigInteger.valueOf(targets.getStatisticalUnits().size()));
        return targets;
    }

    private UnitMeasures toUnitMeasures(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        UnitMeasures targets = new UnitMeasures();
        toResourcesExternalItemsSrm(sources, targets.getUnitMeasures());
        targets.setKind(KIND_SRM_EXTERNAL_ITEM);
        targets.setTotal(BigInteger.valueOf(targets.getUnitMeasures().size()));
        return targets;
    }

    private StatConcDefs toStatConcDefs(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        StatConcDefs targets = new StatConcDefs();
        toResourcesExternalItemsSrm(sources, targets.getStatConcDefs());
        targets.setKind(KIND_SRM_EXTERNAL_ITEM);
        targets.setTotal(BigInteger.valueOf(targets.getStatConcDefs().size()));
        return targets;
    }

    private ClassSystems toClassSystems(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        ClassSystems targets = new ClassSystems();
        toResourcesExternalItemsSrm(sources, targets.getClassSystems());
        targets.setKind(KIND_SRM_EXTERNAL_ITEM);
        targets.setTotal(BigInteger.valueOf(targets.getClassSystems().size()));
        return targets;
    }

    private InformationSuppliers toInformationSuppliers(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        InformationSuppliers targets = new InformationSuppliers();
        toResourcesExternalItemsSrm(sources, targets.getInformationSuppliers());
        targets.setKind(KIND_SRM_EXTERNAL_ITEM);
        targets.setTotal(BigInteger.valueOf(targets.getInformationSuppliers().size()));
        return targets;
    }

    private FreqColls toFreqColls(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        FreqColls targets = new FreqColls();
        toResourcesExternalItemsSrm(sources, targets.getFreqColls());
        targets.setKind(KIND_SRM_EXTERNAL_ITEM);
        targets.setTotal(BigInteger.valueOf(targets.getFreqColls().size()));
        return targets;
    }
}
