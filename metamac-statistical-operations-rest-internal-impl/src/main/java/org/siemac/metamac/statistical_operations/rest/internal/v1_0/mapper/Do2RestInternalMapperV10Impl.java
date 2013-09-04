package org.siemac.metamac.statistical_operations.rest.internal.v1_0.mapper;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Set;

import javax.annotation.PostConstruct;
import javax.ws.rs.core.Response.Status;

import org.apache.commons.lang.StringUtils;
import org.fornax.cartridges.sculptor.framework.domain.PagedResult;
import org.joda.time.DateTime;
import org.siemac.metamac.core.common.conf.ConfigurationService;
import org.siemac.metamac.core.common.constants.shared.ConfigurationConstants;
import org.siemac.metamac.core.common.ent.domain.ExternalItem;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.rest.common.v1_0.domain.ChildLinks;
import org.siemac.metamac.rest.common.v1_0.domain.InternationalString;
import org.siemac.metamac.rest.common.v1_0.domain.Item;
import org.siemac.metamac.rest.common.v1_0.domain.LocalisedString;
import org.siemac.metamac.rest.common.v1_0.domain.ResourceLink;
import org.siemac.metamac.rest.common_metadata.v1_0.domain.Configuration;
import org.siemac.metamac.rest.exception.RestException;
import org.siemac.metamac.rest.exception.utils.RestExceptionUtils;
import org.siemac.metamac.rest.search.criteria.mapper.SculptorCriteria2RestCriteria;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.ClassSystems;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.CollMethods;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.Costs;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.DataSharings;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.Families;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.Family;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.FreqColls;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.GeographicGranularities;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.InformationSuppliers;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.Instance;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.InstanceTypes;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.Instances;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.LegalActs;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.Measures;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.OfficialityTypes;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.Operation;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.Operations;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.ProcStatus;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.Producers;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.Publishers;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.RegionalContributors;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.RegionalResponsibles;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.ResourceInternal;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.SecondarySubjectAreas;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.StatConcDefs;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.StatisticalOperationSources;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.StatisticalOperationTypes;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.StatisticalUnits;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.TemporalGranularities;
import org.siemac.metamac.rest.statistical_operations_internal.v1_0.domain.UpdateFrequencies;
import org.siemac.metamac.rest.utils.RestUtils;
import org.siemac.metamac.srm.rest.common.SrmRestConstants;
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
import org.siemac.metamac.statistical_operations.rest.internal.invocation.SrmRestInternalFacade;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.service.utils.InternalWebApplicationNavigation;
import org.springframework.beans.factory.BeanCreationException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class Do2RestInternalMapperV10Impl implements Do2RestInternalMapperV10 {

    @Autowired
    private ConfigurationService             configurationService;

    @Autowired
    private CommonMetadataRestExternalFacade commonMetadataRestExternalFacade;

    @Autowired
    private SrmRestInternalFacade            srmRestInternalFacade;

    private String                           statisticalOperationsApiInternalEndpointV10;
    private String                           statisticalOperationsInternalWebApplication;
    private String                           srmApiInternalEndpoint;
    private String                           srmInternalWebApplication;

    private InternalWebApplicationNavigation internalWebApplicationNavigation;

    @PostConstruct
    public void init() throws Exception {
        initEndpoints();

        internalWebApplicationNavigation = new InternalWebApplicationNavigation(statisticalOperationsInternalWebApplication);
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
        target.setSelfLink(toOperationSelfLink(source));
        target.setName(toInternationalString(source.getTitle()));
        target.setAcronym(toInternationalString(source.getAcronym()));
        target.setSubjectArea(toResourceExternalItemSrm(source.getSubjectArea()));
        target.setSecondarySubjectAreas(toSecondarySubjectAreas(source.getSecondarySubjectAreas()));
        target.setObjective(toInternationalString(source.getObjective()));
        target.setDescription(toInternationalString(source.getDescription()));
        target.setStatisticalOperationType(toItem(source.getSurveyType()));
        target.setOfficialityType(toItem(source.getOfficialityType()));
        target.setIndicatorSystem(source.getIndicatorSystem());
        target.setProducers(toProducers(source.getProducer()));
        target.setRegionalResponsibles(toRegionalResponsibles(source.getRegionalResponsible()));
        target.setRegionalContributors(toRegionalContributors(source.getRegionalContributor()));
        target.setCreatedDate(toDate(source.getCreatedDate()));
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
        target.setLegalActs(toOperationLegalActs(source.getSpecificLegalActs(), null, target.getLegalActs()));
        target.setDataSharings(toOperationDataSharings(source.getSpecificDataSharing(), null, target.getDataSharings()));
        target.setComment(toInternationalString(source.getComment()));
        target.setNotes(toInternationalString(source.getNotes()));
        target.setParentLink(toOperationParentLink());
        target.setChildLinks(toOperationChildLinks(source));
        target.setManagementAppLink(toOperationManagementApplicationLink(source));
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
            ResourceInternal target = toResource(source);
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
            ResourceInternal target = toResource(source);
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
        target.setSelfLink(toFamilySelfLink(source));
        target.setName(toInternationalString(source.getTitle()));
        target.setAcronym(toInternationalString(source.getAcronym()));
        target.setDescription(toInternationalString(source.getDescription()));
        target.setCreatedDate(toDate(source.getCreatedDate()));
        target.setInternalInventoryDate(toDate(source.getInternalInventoryDate()));
        target.setProcStatus(toProcStatus(source.getProcStatus()));
        target.setInventoryDate(toDate(source.getInventoryDate()));
        target.setParentLink(toFamilyParentLink());
        target.setChildLinks(toFamilyChildLinks(source));
        target.setManagementAppLink(toFamilyManagementApplicationLink(source));
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
            ResourceInternal target = toResource(source);
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
                ResourceInternal target = toResource(source);
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
        target.setSelfLink(toInstanceSelfLink(source));
        target.setName(toInternationalString(source.getTitle()));
        target.setAcronym(toInternationalString(source.getAcronym()));
        target.setStatisticalOperation(toResource(source.getOperation()));
        target.setPredecessor(toResource(getInstanceInOrder(source.getOperation().getInstances(), source.getOrder() - 1)));
        target.setSuccessor(toResource(getInstanceInOrder(source.getOperation().getInstances(), source.getOrder() + 1)));
        target.setDataDescription(toInternationalString(source.getDataDescription()));
        target.setStatisticalPopulation(toInternationalString(source.getStatisticalPopulation()));
        target.setStatisticalUnits(toStatisticalUnits(source.getStatisticalUnit()));
        target.setGeographicGranularity(toGeographicGranularities(source.getGeographicGranularity()));
        target.setGeographicComparability(toInternationalString(source.getGeographicComparability()));
        target.setTemporalGranularity(toTemporalGranularities(source.getTemporalGranularity()));
        target.setTemporalComparability(toInternationalString(source.getTemporalComparability()));
        target.setBasePeriod(source.getBasePeriod());
        target.setMeasures(toMeasures(source.getUnitMeasure()));
        target.setStatConcDefsDescription(toInternationalString(source.getStatConcDef()));
        target.setStatConcDefs(toStatConcDefs(source.getStatConcDefList()));
        target.setClassSystemsDescription(toInternationalString(source.getClassSystem()));
        target.setClassSystems(toClassSystems(source.getClassSystemList()));
        target.setInstanceType(toItem(source.getInstanceType()));
        target.setCreatedDate(toDate(source.getCreatedDate()));
        target.setInternalInventoryDate(toDate(source.getInternalInventoryDate()));
        target.setProcStatus(toProcStatus(source.getProcStatus()));
        target.setDocMethod(toInternationalString(source.getDocMethod()));
        target.setStatisticalOperationSource(toItem(source.getSurveySource()));
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
        target.setParentLink(toInstanceParentLink(source));
        target.setChildLinks(toInstanceChildLinks(source));
        target.setManagementAppLink(toInstanceManagementApplicationLink(source));
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
            ResourceInternal target = toResource(source);
            targets.getInstances().add(target);
        }
        return targets;
    }

    @Override
    public StatisticalOperationTypes toStatisticalOperationTypes(List<SurveyType> sources) {
        StatisticalOperationTypes targets = new StatisticalOperationTypes();
        targets.setKind(RestInternalConstants.KIND_STATISTICAL_OPERATION_TYPES);

        if (sources == null) {
            targets.setTotal(BigInteger.ZERO);
        } else {
            for (org.siemac.metamac.statistical.operations.core.domain.SurveyType source : sources) {
                Item target = toItem(source);
                targets.getStatisticalOperationTypes().add(target);
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
    public StatisticalOperationSources toStatisticalOperationSources(List<SurveySource> sources) {
        StatisticalOperationSources targets = new StatisticalOperationSources();
        targets.setKind(RestInternalConstants.KIND_STATISTICAL_OPERATION_SOURCES);

        if (sources == null) {
            targets.setTotal(BigInteger.ZERO);
        } else {
            for (org.siemac.metamac.statistical.operations.core.domain.SurveySource source : sources) {
                Item target = toItem(source);
                targets.getStatisticalOperationSources().add(target);
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
        Configuration configuration = commonMetadataRestExternalFacade.retrieveConfigurationById(commonMetadata.getCode());

        // Transform
        target.setContact(commonMetadataResourceInternalToResourceInternal(configuration.getContact()));
        target.setLegalActs(toOperationLegalActs(null, configuration.getLegalActs(), target.getLegalActs()));
        target.setDataSharings(toOperationDataSharings(null, configuration.getDataSharing(), target.getDataSharings()));
        target.setConfidentialityPolicy(configuration.getConfPolicy());
        target.setConfidentialityDataTreatment(configuration.getConfDataTreatment());
    }

    private LegalActs toOperationLegalActs(org.siemac.metamac.core.common.ent.domain.InternationalString source1, InternationalString source2, LegalActs target) {
        if (source1 == null && source2 == null) {
            return target; // it can have already other internationalString
        }
        if (target == null) {
            target = new LegalActs();
            target.setTotal(BigInteger.ZERO);
        }
        if (source1 != null) {
            target.getLegalActs().add(toInternationalString(source1));
            target.setTotal(target.getTotal().add(BigInteger.ONE));
        }
        if (source2 != null) {
            target.getLegalActs().add(source2);
            target.setTotal(target.getTotal().add(BigInteger.ONE));
        }
        return target;
    }

    private DataSharings toOperationDataSharings(org.siemac.metamac.core.common.ent.domain.InternationalString source1, InternationalString source2, DataSharings target) {
        if (source1 == null && source2 == null) {
            return target; // it can have already other internationalString
        }
        if (target == null) {
            target = new DataSharings();
            target.setTotal(BigInteger.ZERO);
        }
        if (source1 != null) {
            target.getDataSharings().add(toInternationalString(source1));
            target.setTotal(target.getTotal().add(BigInteger.ONE));
        }
        if (source2 != null) {
            target.getDataSharings().add(source2);
            target.setTotal(target.getTotal().add(BigInteger.ONE));
        }
        return target;
    }

    private ResourceInternal toResource(org.siemac.metamac.statistical.operations.core.domain.Operation source) {
        if (source == null) {
            return null;
        }
        ResourceInternal target = new ResourceInternal();
        target.setId(source.getCode());
        target.setUrn(source.getUrn());
        target.setKind(RestInternalConstants.KIND_OPERATION);
        target.setSelfLink(toOperationSelfLink(source));
        target.setManagementAppLink(toOperationManagementApplicationLink(source));
        target.setName(toInternationalString(source.getTitle()));
        return target;
    }

    private ResourceInternal toResource(org.siemac.metamac.statistical.operations.core.domain.Family source) {
        if (source == null) {
            return null;
        }
        ResourceInternal target = new ResourceInternal();
        target.setId(source.getCode());
        target.setUrn(source.getUrn());
        target.setKind(RestInternalConstants.KIND_FAMILY);
        target.setManagementAppLink(toFamilyManagementApplicationLink(source));
        target.setSelfLink(toFamilySelfLink(source));
        target.setName(toInternationalString(source.getTitle()));
        return target;
    }

    private ResourceInternal toResource(org.siemac.metamac.statistical.operations.core.domain.Instance source) {
        if (source == null) {
            return null;
        }
        ResourceInternal target = new ResourceInternal();
        target.setId(source.getCode());
        target.setUrn(source.getUrn());
        target.setKind(RestInternalConstants.KIND_INSTANCE);
        target.setSelfLink(toInstanceSelfLink(source));
        target.setManagementAppLink(toInstanceManagementApplicationLink(source));
        target.setName(toInternationalString(source.getTitle()));
        return target;
    }

    private Item toItem(org.siemac.metamac.statistical.operations.core.domain.SurveyType source) {
        if (source == null) {
            return null;
        }
        Item target = new Item();
        target.setId(source.getIdentifier());
        target.setName(toInternationalString(source.getDescription()));
        return target;
    }

    private Item toItem(org.siemac.metamac.statistical.operations.core.domain.OfficialityType source) {
        if (source == null) {
            return null;
        }
        Item target = new Item();
        target.setId(source.getIdentifier());
        target.setName(toInternationalString(source.getDescription()));
        return target;
    }

    private Item toItem(org.siemac.metamac.statistical.operations.core.domain.SurveySource source) {
        if (source == null) {
            return null;
        }
        Item target = new Item();
        target.setId(source.getIdentifier());
        target.setName(toInternationalString(source.getDescription()));
        return target;
    }

    private Item toItem(org.siemac.metamac.statistical.operations.core.domain.InstanceType source) {
        if (source == null) {
            return null;
        }
        Item target = new Item();
        target.setId(source.getIdentifier());
        target.setName(toInternationalString(source.getDescription()));
        return target;
    }

    private Item toItem(org.siemac.metamac.statistical.operations.core.domain.CollMethod source) {
        if (source == null) {
            return null;
        }
        Item target = new Item();
        target.setId(source.getIdentifier());
        target.setName(toInternationalString(source.getDescription()));
        return target;
    }

    private Item toItem(org.siemac.metamac.statistical.operations.core.domain.Cost source) {
        if (source == null) {
            return null;
        }
        Item target = new Item();
        target.setId(source.getIdentifier());
        target.setName(toInternationalString(source.getDescription()));
        return target;
    }

    private void toResourcesExternalItems(Set<ExternalItem> sources, List<ResourceInternal> targets, String apiExternalItem, String managementAppBaseUrl) {
        if (sources == null) {
            return;
        }
        for (ExternalItem source : sources) {
            ResourceInternal target = toResourceExternalItem(source, apiExternalItem, managementAppBaseUrl);
            targets.add(target);
        }
    }

    private void toResourcesExternalItemsSrm(Set<ExternalItem> sources, List<ResourceInternal> targets) {
        toResourcesExternalItems(sources, targets, srmApiInternalEndpoint, srmInternalWebApplication);
    }

    private ResourceInternal toResourceExternalItemSrm(ExternalItem source) {
        if (source == null) {
            return null;
        }
        return toResourceExternalItem(source, srmApiInternalEndpoint, srmInternalWebApplication);
    }

    private ResourceInternal toResourceExternalItem(ExternalItem source, String apiExternalItemBase, String managementAppBaseUrl) {
        if (source == null) {
            return null;
        }
        ResourceInternal target = new ResourceInternal();
        target.setId(source.getCode());
        target.setNestedId(source.getCodeNested());
        target.setUrn(source.getUrn());
        target.setKind(source.getType().getValue());
        target.setSelfLink(toResourceLink(target.getKind(), RestUtils.createLink(apiExternalItemBase, source.getUri())));
        if (source.getManagementAppUrl() != null) {
            target.setManagementAppLink(RestUtils.createLink(managementAppBaseUrl, source.getManagementAppUrl()));
        }
        target.setName(toInternationalString(source.getTitle()));
        return target;
    }

    private ResourceLink toOperationSelfLink(org.siemac.metamac.statistical.operations.core.domain.Operation source) {
        return toResourceLink(RestInternalConstants.KIND_OPERATION, toOperationLink(source));
    }

    private ResourceLink toOperationParentLink() {
        return toResourceLink(RestInternalConstants.KIND_OPERATIONS, toOperationsLink());
    }

    private ChildLinks toOperationChildLinks(org.siemac.metamac.statistical.operations.core.domain.Operation operation) {
        ChildLinks targets = new ChildLinks();

        targets.getChildLinks().add(toResourceLink(RestInternalConstants.KIND_INSTANCES, toInstancesLink(operation)));
        targets.getChildLinks().add(toResourceLink(RestInternalConstants.KIND_FAMILIES, toFamiliesByOperationLink(operation)));

        targets.setTotal(BigInteger.valueOf(targets.getChildLinks().size()));
        return targets;
    }

    private ResourceLink toFamilySelfLink(org.siemac.metamac.statistical.operations.core.domain.Family source) {
        return toResourceLink(RestInternalConstants.KIND_FAMILY, toFamilyLink(source));
    }

    private ResourceLink toFamilyParentLink() {
        return toResourceLink(RestInternalConstants.KIND_FAMILIES, toFamiliesLink());
    }

    private ChildLinks toFamilyChildLinks(org.siemac.metamac.statistical.operations.core.domain.Family family) {
        ChildLinks targets = new ChildLinks();

        // Operations of family
        targets.getChildLinks().add(toResourceLink(RestInternalConstants.KIND_OPERATIONS, toOperationsByFamilyLink(family)));

        targets.setTotal(BigInteger.valueOf(targets.getChildLinks().size()));
        return targets;
    }

    private ResourceLink toInstanceSelfLink(org.siemac.metamac.statistical.operations.core.domain.Instance instance) {
        return toResourceLink(RestInternalConstants.KIND_INSTANCE, toInstanceLink(instance));
    }

    private ResourceLink toInstanceParentLink(org.siemac.metamac.statistical.operations.core.domain.Instance instance) {
        return toResourceLink(RestInternalConstants.KIND_OPERATION, toOperationLink(instance.getOperation()));
    }

    private ChildLinks toInstanceChildLinks(org.siemac.metamac.statistical.operations.core.domain.Instance instance) {
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
    private String toOperationLink(String operationCode) {
        String linkOperations = toOperationsLink();
        return RestUtils.createLink(linkOperations, operationCode);
    }
    private String toOperationLink(org.siemac.metamac.statistical.operations.core.domain.Operation operation) {
        return toOperationLink(operation.getCode());
    }

    // API/operations/OPERATION_ID/instances
    private String toInstancesLink(org.siemac.metamac.statistical.operations.core.domain.Operation operation) {
        String linkOperation = null;
        if (operation != null) {
            linkOperation = toOperationLink(operation);
        } else {
            linkOperation = toOperationLink(RestInternalConstants.WILDCARD_ALL);
        }
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
                return ProcStatus.INTERNALLY_PUBLISHED;
            case PUBLISH_EXTERNALLY:
                return ProcStatus.EXTERNALLY_PUBLISHED;
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
        targets.setKind(SrmRestConstants.KIND_CATEGORIES);
        targets.setTotal(BigInteger.valueOf(targets.getSecondarySubjectAreas().size()));
        return targets;
    }

    private Producers toProducers(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        Producers targets = new Producers();
        toResourcesExternalItemsSrm(sources, targets.getProducers());
        targets.setKind(SrmRestConstants.KIND_ORGANISATION_UNITS);
        targets.setTotal(BigInteger.valueOf(targets.getProducers().size()));
        return targets;
    }

    private RegionalResponsibles toRegionalResponsibles(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        RegionalResponsibles targets = new RegionalResponsibles();
        toResourcesExternalItemsSrm(sources, targets.getRegionalResponsibles());
        targets.setKind(SrmRestConstants.KIND_ORGANISATION_UNITS);
        targets.setTotal(BigInteger.valueOf(targets.getRegionalResponsibles().size()));
        return targets;
    }

    private RegionalContributors toRegionalContributors(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        RegionalContributors targets = new RegionalContributors();
        toResourcesExternalItemsSrm(sources, targets.getRegionalContributors());
        targets.setKind(SrmRestConstants.KIND_ORGANISATIONS);
        targets.setTotal(BigInteger.valueOf(targets.getRegionalContributors().size()));
        return targets;
    }

    private Publishers toPublishers(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        Publishers targets = new Publishers();
        toResourcesExternalItemsSrm(sources, targets.getPublishers());
        targets.setKind(SrmRestConstants.KIND_ORGANISATION_UNITS);
        targets.setTotal(BigInteger.valueOf(targets.getPublishers().size()));
        return targets;
    }

    private UpdateFrequencies toUpdateFrequencies(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        UpdateFrequencies targets = new UpdateFrequencies();
        toResourcesExternalItemsSrm(sources, targets.getUpdateFrequencies());
        targets.setKind(SrmRestConstants.KIND_CODES);
        targets.setTotal(BigInteger.valueOf(targets.getUpdateFrequencies().size()));
        return targets;
    }

    private StatisticalUnits toStatisticalUnits(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        StatisticalUnits targets = new StatisticalUnits();
        toResourcesExternalItemsSrm(sources, targets.getStatisticalUnits());
        targets.setKind(SrmRestConstants.KIND_CONCEPTS);
        targets.setTotal(BigInteger.valueOf(targets.getStatisticalUnits().size()));
        return targets;
    }

    private GeographicGranularities toGeographicGranularities(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        GeographicGranularities targets = new GeographicGranularities();
        toResourcesExternalItemsSrm(sources, targets.getGeographicGranularities());
        targets.setKind(SrmRestConstants.KIND_CODES);
        targets.setTotal(BigInteger.valueOf(targets.getGeographicGranularities().size()));
        return targets;
    }

    private TemporalGranularities toTemporalGranularities(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        TemporalGranularities targets = new TemporalGranularities();
        toResourcesExternalItemsSrm(sources, targets.getTemporalGranularities());
        targets.setKind(SrmRestConstants.KIND_CODES);
        targets.setTotal(BigInteger.valueOf(targets.getTemporalGranularities().size()));
        return targets;
    }

    private Measures toMeasures(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        Measures targets = new Measures();
        toResourcesInternalFromExternalItemWithConceptSchemesAndConcepts(sources, targets.getMeasures());
        targets.setKind(SrmRestConstants.KIND_CONCEPTS);
        targets.setTotal(BigInteger.valueOf(targets.getMeasures().size()));
        return targets;
    }

    private StatConcDefs toStatConcDefs(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        StatConcDefs targets = new StatConcDefs();
        toResourcesInternalFromExternalItemWithConceptSchemesAndConcepts(sources, targets.getStatConcDefs());
        targets.setKind(SrmRestConstants.KIND_CONCEPTS);
        targets.setTotal(BigInteger.valueOf(targets.getStatConcDefs().size()));
        return targets;
    }

    private void toResourcesInternalFromExternalItemWithConceptSchemesAndConcepts(Set<ExternalItem> sources, List<ResourceInternal> targets) {
        if (sources == null || sources.size() == 0) {
            return;
        }
        for (ExternalItem source : sources) {
            if (TypeExternalArtefactsEnum.CONCEPT.equals(source.getType())) {
                ResourceInternal target = toResourceExternalItemSrm(source);
                targets.add(target);
            } else if (TypeExternalArtefactsEnum.CONCEPT_SCHEME.equals(source.getType())) {
                List<ResourceInternal> targetConcepts = srmConceptSchemeToResourceInternalConcepts(source);
                targets.addAll(targetConcepts);
            } else {
                org.siemac.metamac.rest.common.v1_0.domain.Exception exception = RestExceptionUtils.getException(RestServiceExceptionType.UNKNOWN);
                throw new RestException(exception, Status.INTERNAL_SERVER_ERROR);
            }
        }
    }

    private ClassSystems toClassSystems(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        ClassSystems targets = new ClassSystems();
        toResourcesExternalItemsSrm(sources, targets.getClassSystems());
        targets.setKind(SrmRestConstants.KIND_CODELISTS);
        targets.setTotal(BigInteger.valueOf(targets.getClassSystems().size()));
        return targets;
    }

    private InformationSuppliers toInformationSuppliers(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        InformationSuppliers targets = new InformationSuppliers();
        toResourcesExternalItemsSrm(sources, targets.getInformationSuppliers());
        targets.setKind(SrmRestConstants.KIND_DATA_PROVIDERS);
        targets.setTotal(BigInteger.valueOf(targets.getInformationSuppliers().size()));
        return targets;
    }

    private FreqColls toFreqColls(Set<ExternalItem> sources) {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        FreqColls targets = new FreqColls();
        toResourcesExternalItemsSrm(sources, targets.getFreqColls());
        targets.setKind(SrmRestConstants.KIND_CODES);
        targets.setTotal(BigInteger.valueOf(targets.getFreqColls().size()));
        return targets;
    }

    private ResourceLink toResourceLink(String kind, String href) {
        ResourceLink target = new ResourceLink();
        target.setKind(kind);
        target.setHref(href);
        return target;
    }

    private String readProperty(String property) {
        String propertyValue = configurationService.getProperty(property);
        if (propertyValue == null) {
            throw new BeanCreationException("Property not found: " + property);
        }
        return propertyValue;
    }

    private void initEndpoints() {
        // Srm internal application
        srmInternalWebApplication = readProperty(ConfigurationConstants.WEB_APPLICATION_SRM_INTERNAL_WEB);
        srmInternalWebApplication = StringUtils.removeEnd(srmInternalWebApplication, "/");

        // Statistical operations internal application
        statisticalOperationsInternalWebApplication = readProperty(ConfigurationConstants.WEB_APPLICATION_STATISTICAL_OPERATIONS_INTERNAL_WEB);
        statisticalOperationsInternalWebApplication = StringUtils.removeEnd(statisticalOperationsInternalWebApplication, "/");

        // Statistical operations Internal Api v1.0
        String statisticalOperationsApiInternalEndpoint = readProperty(ConfigurationConstants.ENDPOINT_STATISTICAL_OPERATIONS_INTERNAL_API);
        statisticalOperationsApiInternalEndpointV10 = RestUtils.createLink(statisticalOperationsApiInternalEndpoint, RestInternalConstants.API_VERSION_1_0);

        // Srm Internal Api (do not add api version! it is already stored in database)
        srmApiInternalEndpoint = readProperty(ConfigurationConstants.ENDPOINT_SRM_INTERNAL_API);
    }

    private String toOperationManagementApplicationLink(org.siemac.metamac.statistical.operations.core.domain.Operation source) {
        return internalWebApplicationNavigation.buildOperationUrl(source);
    }

    private String toFamilyManagementApplicationLink(org.siemac.metamac.statistical.operations.core.domain.Family source) {
        return internalWebApplicationNavigation.buildFamilyUrl(source);
    }

    private String toInstanceManagementApplicationLink(org.siemac.metamac.statistical.operations.core.domain.Instance source) {
        return internalWebApplicationNavigation.buildInstanceUrl(source);
    }

    private List<ResourceInternal> srmConceptSchemeToResourceInternalConcepts(ExternalItem conceptSchemeSource) {
        // Return from API
        List<org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.ItemResourceInternal> conceptSources = srmRestInternalFacade.retrieveConceptsByConceptScheme(conceptSchemeSource
                .getUrn());

        // Transform
        List<ResourceInternal> targets = new ArrayList<ResourceInternal>(conceptSources.size());
        for (org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.ResourceInternal conceptSource : conceptSources) {
            ResourceInternal conceptTarget = srmResourceInternalToResourceInternal(conceptSource);
            targets.add(conceptTarget);
        }
        return targets;
    }

    private ResourceInternal commonMetadataResourceInternalToResourceInternal(org.siemac.metamac.rest.common_metadata.v1_0.domain.ResourceInternal source) {
        if (source == null) {
            return null;
        }
        ResourceInternal target = new ResourceInternal();
        target.setId(source.getId());
        target.setUrn(source.getUrn());
        target.setKind(source.getKind());
        target.setSelfLink(source.getSelfLink());
        target.setManagementAppLink(source.getManagementAppLink());
        target.setName(source.getName());
        return target;
    }

    private ResourceInternal srmResourceInternalToResourceInternal(org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.ResourceInternal source) {
        if (source == null) {
            return null;
        }
        ResourceInternal target = new ResourceInternal();
        target.setId(source.getId());
        target.setUrn(source.getUrn());
        target.setKind(source.getKind());
        target.setSelfLink(source.getSelfLink());
        target.setManagementAppLink(source.getManagementAppLink());
        target.setName(source.getName());
        return target;
    }

}
